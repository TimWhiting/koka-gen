// ---------------------------------------------------------
// 1. Types
// ---------------------------------------------------------

// A task is a prepared computation that runs in the scheduler loop.
// It has 'div' (loops) and 'local' (updates scheduler state).
// We parameterize over 'e' to allow arbitrary user effects (like console).
value struct task<e::E>
  run : () -> <div|e> ()

// A waiter is a captured continuation expecting an int message.
// It returns the next task function.
value struct waiter<e::E>
  callback : (int) -> <div|e> ()

// The communication effect.
// We use 'div effect' to allow recursion.
// We parameterize over 'e' so that 'spawn' can accept actions with effect 'e'.
div effect comm<e::E>
  ctl spawn( action : () -> <comm<e>,div|e> () ) : ()
  ctl yield() : ()
  ctl send( chan : string, v : int ) : ()
  ctl recv( chan : string ) : int
  ctl shutdown() : ()

// ---------------------------------------------------------
// 2. The Scheduler
// ---------------------------------------------------------

fun schedule( main1 : () -> <comm<e>,div|e> () ) : <div|e> ()
  
  // --- Runtime State ---
  var ready : some<e,h> list<task<<local<h>|e>>> := []
  var mailboxes : list<(string, list<int>)> := []
  var waiting : some<e,h> list<(string, list<waiter<<local<h>|e>>>)> := []
  var done := False
  // --- Helpers ---
  fun enqueue( t : some<e,h> task<<local<h>|e>> )
    ready := ready ++ [t]

  fun find-map( l : list<(string, a)>, key : string ) : maybe<a>
    match l.find( fn(p) p.fst == key )
      Just((_, v)) -> Just(v)
      Nothing      -> Nothing

  // --- The Run Loop ---
  fun run-loop() : _ ()
    match ready
      Nil -> () 
      Cons(current-task, rest) -> 
        ready := rest
        current-task.run.()

  // --- The Handler ---
  // Converts a <comm> action into a task function
  fun handle-comm(action)
    handle({mask<local>(action)})
      return(_) ()
      ctl shutdown()
        ready := []
        done := True
      ctl yield()
        // val k = resume(()) // k is () -> (() -> ...)
        enqueue( Task(fn() resume(())) )

      ctl spawn(child)
        // Defer both child and parent - don't run resume now
        val child-task = fn() handle-comm(child)
        val parent-task = fn() { val k = resume(()); k() }
        enqueue( Task(child-task) )
        enqueue( Task(parent-task) )
      ctl send(chan, msg)
        val waiters = waiting.find-map(chan).default([])
        match waiters
          Cons(w, w-rest) ->
            waiting := waiting.filter(fn(p) p.fst != chan) ++ [(chan, w-rest)]
            // w.callback(msg) returns the next task function for the waiter
            val waiter-task = fn() w.callback.(msg)
            enqueue( Task(waiter-task) )
            // Defer sender too - let waiter run first
            val sender-task = fn() { val k = resume(()); k() }
            enqueue( Task(sender-task) )
          Nil ->
            val msgs = mailboxes.find-map(chan).default([])
            mailboxes := mailboxes.filter(fn(p) p.fst != chan) ++ [(chan, msgs ++ [msg])]
            resume(())
      ctl recv(chan)
        val msgs = mailboxes.find-map(chan).default([])
        match msgs
          Cons(m, m-rest) ->
            mailboxes := mailboxes.filter(fn(p) p.fst != chan) ++ [(chan, m-rest)]
            resume(m)
          Nil ->
            // Capture continuation
            // resume is int -> (() -> ...)
            val k = fn(x){ resume(x) }
            val w = Waiter(k)
            val ws = waiting.find-map(chan).default([])
            waiting := waiting.filter(fn(p) p.fst != chan) ++ [(chan, ws ++ [w])]
            // Block: return run-loop to continue with next task

  // --- Bootstrap ---
  val main-task = handle-comm(main1)
  enqueue( Task(fn() main-task()) )
  while {!done}
    run-loop()

// ---------------------------------------------------------
// 3. Application Code
// ---------------------------------------------------------

fun main()
  test-yield()
  test-spawn()
  test-send-recv()
  test-send-recv-buffered()
  test-ping-pong()
  test-custom-effect-logger()
  test-custom-effect-exception()
  test-prime-sieve()

// ---------------------------------------------------------
// Test: yield - basic cooperative yielding
// ---------------------------------------------------------
fun test-yield()
  println("\n--- Test: yield ---")
  schedule fn()
    println("Task A: step 1")
    yield()
    println("Task A: step 2")
    yield()
    println("Task A: step 3")
    shutdown()

// ---------------------------------------------------------
// Test: spawn - spawning multiple tasks
// ---------------------------------------------------------
fun test-spawn()
  println("\n--- Test: spawn ---")
  schedule fn()
    println("Parent: starting")
    spawn fn()
      println("Child 1: running")
    spawn fn()
      println("Child 2: running")
    println("Parent: after spawns")
    yield()
    println("Parent: done")
    shutdown()

// ---------------------------------------------------------
// Test: send/recv - basic channel communication
// ---------------------------------------------------------
fun test-send-recv()
  println("\n--- Test: send/recv ---")
  schedule fn()
    spawn fn()
      println("Sender: sending 42")
      send("ch", 42)
      println("Sender: sending 100")
      send("ch", 100)
    spawn fn()
      val x = recv("ch")
      println("Receiver: got " ++ x.show)
      val y = recv("ch")
      println("Receiver: got " ++ y.show)
      shutdown()

// ---------------------------------------------------------
// Test: send before recv - message buffering
// ---------------------------------------------------------
fun test-send-recv-buffered()
  println("\n--- Test: send/recv buffered ---")
  schedule fn()
    // Send messages before receiver starts
    send("buf-ch", 1)
    send("buf-ch", 2)
    send("buf-ch", 3)
    println("Sent 3 messages to buffer")
    spawn fn()
      val a = recv("buf-ch")
      val b = recv("buf-ch")
      val c = recv("buf-ch")
      println("Received: " ++ a.show ++ ", " ++ b.show ++ ", " ++ c.show)
      shutdown()

// ---------------------------------------------------------
// Test: ping-pong - bidirectional communication
// ---------------------------------------------------------
fun test-ping-pong()
  println("\n--- Test: ping-pong ---")
  schedule fn()
    spawn fn()
      // Ping
      for(1, 3) fn(i)
        println("Ping: sending " ++ i.show)
        send("ping", i)
        val reply = recv("pong")
        println("Ping: got reply " ++ reply.show)
      shutdown()
    spawn fn()
      // Pong
      while { True }
        val x = recv("ping")
        println("Pong: got " ++ x.show ++ ", replying " ++ (x * 10).show)
        send("pong", x * 10)

// ---------------------------------------------------------
// Test: Custom effect - Logger
// Demonstrates that the scheduler is polymorphic over user effects
// ---------------------------------------------------------
effect logger
  fun log(msg : string) : ()

fun test-custom-effect-logger()
  println("\n--- Test: custom effect (logger) ---")
  var log-buffer := []
  with handler
    fun log(msg) log-buffer := log-buffer ++ [msg]
  schedule fn()
    log("Task 1: starting")
    spawn fn()
      log("Task 2: running")
      send("log-ch", 42)
      log("Task 2: sent message")
    val x = recv("log-ch")
    log("Task 1: received " ++ x.show)
    shutdown()
  println("Log buffer: " ++ log-buffer.join(", "))

// ---------------------------------------------------------
// Test: Custom effect - Exception that exits scheduler
// Demonstrates that exceptions propagate out of the scheduler
// ---------------------------------------------------------
effect ctl bail-out(reason : string) : ()

fun test-custom-effect-exception()
  println("\n--- Test: custom effect (exception) ---")
  fun run-scheduler() : <bail-out,console,div> string
    schedule fn()
      println("Task: step 1")
      spawn fn()
        println("Child: checking condition...")
        val x = recv("check-ch")
        if x > 100 then
          println("Child: value too large, bailing out!")
          bail-out("value " ++ x.show ++ " exceeds limit")
        println("Child: value ok")
        shutdown()
      send("check-ch", 999)  // This will trigger bail-out
      println("Task: waiting...")
    "Completed normally"
  val result = 
    with handler
      ctl bail-out(reason) -> "Bailed out: " ++ reason
    run-scheduler()
  println("Result: " ++ result)

// ---------------------------------------------------------
// Test: Prime Sieve (the original example)
// ---------------------------------------------------------
fun test-prime-sieve()
  println("\n--- Test: Prime Sieve ---")
  schedule fn()
    val start-ch = "ch-2"
    println("Found Prime: 2")
    spawn( fn(){ filter-actor( start-ch, 2 ) } )
    spawn( fn(){ generator( start-ch, 30 ) } )

// Note: We infer the effect 'e' to be 'console' here because we use println.
fun filter-actor( in-ch : string, my-prime : int ) : <comm<<console>>,console,div> ()
  val out-ch = "ch-" ++ (my-prime + 1).show
  var has-neighbor := False

  while { True }
    val n = recv(in-ch)
    if n % my-prime != 0 then
      if !has-neighbor then
        println("Found Prime: " ++ n.show)
        has-neighbor := True
        spawn( fn(){ filter-actor( out-ch, n ) } )
      else
        send(out-ch, n)

fun generator( start-ch : string, limit : int ) : <comm<<console>>,console,div> ()
  for( 2, limit ) fn(i)
    send(start-ch, i)
    yield() 
  println("Generator finished.")
  shutdown()
