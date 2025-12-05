// ---------------------------------------------------------
// 1. Types
// ---------------------------------------------------------

// A task is a prepared computation that runs in the scheduler loop.
value struct task<e::E>
  run : () -> <div|e> ()

// A typed channel - the phantom type 'a' tracks what messages it carries
abstract struct chan<a>
  name : string

// Create a typed channel
pub fun channel( name : string ) : chan<a>
  Chan(name)

// Get the name of a channel for lookup
pub fun chan-name( c : chan<a> ) : string
  c.name

// A waiter expecting a message of type 'a'
value struct waiter<e::E,a>
  callback : (a) -> <div|e> ()

// A channel-state bundles the mailbox and waiters for one channel.
// The type 'a' is existentially hidden but consistent within this entry.
value struct channel-state<e::E>
  key : string
  mbox : some<a> list<a>
  waiters-callback : some<a> list<(a) -> <div|e> ()>

// The communication effect with typed channels
div effect comm<e::E>
  ctl spawn( action : () -> <comm<e>,div|e> () ) : ()
  ctl yield() : ()
  ctl send( ch : chan<a>, v : a ) : ()
  ctl recv( ch : chan<a> ) : a
  ctl shutdown() : ()

// ---------------------------------------------------------
// 2. The Scheduler
// ---------------------------------------------------------

fun schedule( main1 : () -> <comm<e>,div|e> () ) : <div|e> ()
  
  // --- Runtime State ---
  var ready : some<e,h> list<task<<local<h>|e>>> := []
  // Each channel-state bundles key, mailbox, and waiters with hidden but consistent type
  var channels : some<e,h> list<channel-state<<local<h>|e>>> := []
  var done := False
  
  // --- Helpers ---
  fun enqueue( t : some<e,h> task<<local<h>|e>> )
    ready := ready ++ [t]

  fun find-channel( chkey : string ) : _ maybe<channel-state<_>>
    channels.find( fn(cs) cs.key == chkey )

  fun update-channel( chkey : string, new-state : channel-state<_> ) : _ ()
    channels := channels.filter( fn(cs) cs.key != chkey ) ++ [new-state]

  // --- The Run Loop ---
  fun run-loop() : _ ()
    match ready
      Nil -> () 
      Cons(current-task, rest) -> 
        ready := rest
        current-task.run.()

  // --- The Handler ---
  fun handle-comm(action)
    handle({mask<local>(action)})
      return(_) ()
      ctl shutdown()
        ready := []
        done := True
      ctl yield()
        enqueue( Task(fn() resume(())) )

      ctl spawn(child)
        val child-task = fn() handle-comm(child)
        val parent-task = fn() { val k = resume(()); k() }
        enqueue( Task(child-task) )
        enqueue( Task(parent-task) )
        
      ctl send(ch, msg)
        val key = ch.name
        match find-channel(key)
          Just(cs) ->
            match cs.waiters-callback
              Cons(w, w-rest) ->
                // Wake up the waiter with the message
                update-channel(key, Channel-state(key, cs.mbox, w-rest))
                val waiter-task = fn() w(msg)
                enqueue( Task(waiter-task) )
                val sender-task = fn() { val k = resume(()); k() }
                enqueue( Task(sender-task) )
              Nil ->
                // No waiter, buffer the message
                update-channel(key, Channel-state(key, cs.mbox ++ [msg], cs.waiters-callback))
                resume(())
          Nothing ->
            // Create new channel state with message in mailbox
            channels := channels ++ [Channel-state(key, [msg], [])]
            resume(())
            
      ctl recv(ch)
        val key = ch.name
        match find-channel(key)
          Just(cs) ->
            match cs.mbox
              Cons(m, m-rest) ->
                // Got a message from mailbox
                update-channel(key, Channel-state(key, m-rest, cs.waiters-callback))
                resume(m)
              Nil ->
                // No message, register as waiter
                val k = fn(x) resume(x)
                update-channel(key, Channel-state(key, cs.mbox, cs.waiters-callback ++ [k]))
          Nothing ->
            // Create new channel state and register as waiter
            val k = fn(x) resume(x)
            channels := channels ++ [Channel-state(key, [], [k])]

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
  test-send-recv-polymorphic()
  // test-send-recv-wrong-type()  // Now a compile error - removed!
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
  val ch : chan<int> = channel("ch")
  schedule fn()
    spawn fn()
      println("Sender: sending 42")
      send(ch, 42)
      println("Sender: sending 100")
      send(ch, 100)
    spawn fn()
      val x = recv(ch)
      println("Receiver: got " ++ x.show)
      val y = recv(ch)
      println("Receiver: got " ++ y.show)
      shutdown()

// ---------------------------------------------------------
// Test: send before recv - message buffering
// ---------------------------------------------------------
fun test-send-recv-buffered()
  println("\n--- Test: send/recv buffered ---")
  val buf-ch : chan<int> = channel("buf-ch")
  schedule fn()
    // Send messages before receiver starts
    send(buf-ch, 1)
    send(buf-ch, 2)
    send(buf-ch, 3)
    println("Sent 3 messages to buffer")
    spawn fn()
      val a = recv(buf-ch)
      val b = recv(buf-ch)
      val c = recv(buf-ch)
      println("Received: " ++ a.show ++ ", " ++ b.show ++ ", " ++ c.show)
      shutdown()

// ---------------------------------------------------------
// Test: send/recv with different types - polymorphic channels
// ---------------------------------------------------------
fun test-send-recv-polymorphic()
  println("\n--- Test: send/recv polymorphic ---")
  val str-ch : chan<string> = channel("str-ch")
  val bool-ch : chan<bool> = channel("bool-ch")
  val tuple-ch : chan<(int, string)> = channel("tuple-ch")
  schedule fn()
    // Send strings on one channel
    spawn fn()
      send(str-ch, "hello")
      send(str-ch, "world")
    // Send booleans on another channel  
    spawn fn()
      send(bool-ch, True)
      send(bool-ch, False)
    // Send tuples on a third channel
    spawn fn()
      send(tuple-ch, (1, "one"))
      send(tuple-ch, (2, "two"))
    // Receive and print all
    spawn fn()
      val s1 = recv(str-ch)
      val s2 = recv(str-ch)
      println("Strings: " ++ s1 ++ ", " ++ s2)
      val b1 = recv(bool-ch)
      val b2 = recv(bool-ch)
      println("Bools: " ++ b1.show ++ ", " ++ b2.show)
      val t1 = recv(tuple-ch)
      val t2 = recv(tuple-ch)
      println("Tuples: " ++ t1.show ++ ", " ++ t2.show)
      shutdown()

// ---------------------------------------------------------
// Test: send/recv with WRONG type - NOW A COMPILE ERROR!
// ---------------------------------------------------------
// fun test-send-recv-wrong-type()
//   println("\n--- Test: send/recv WRONG TYPE ---")
//   val ch : chan<string> = channel("mixed-ch")
//   schedule fn()
//     send(ch, "hello")
//     spawn fn()
//       // This would now be a compile error!
//       // val x : int = recv(ch)  // ERROR: chan<string> vs int
//       shutdown()

// ---------------------------------------------------------
// Test: ping-pong - bidirectional communication
// ---------------------------------------------------------
fun test-ping-pong()
  println("\n--- Test: ping-pong ---")
  val ping-ch : chan<int> = channel("ping")
  val pong-ch : chan<int> = channel("pong")
  schedule fn()
    spawn fn()
      // Ping
      for(1, 3) fn(i)
        println("Ping: sending " ++ i.show)
        send(ping-ch, i)
        val reply = recv(pong-ch)
        println("Ping: got reply " ++ reply.show)
      shutdown()
    spawn fn()
      // Pong
      while { True }
        val x = recv(ping-ch)
        println("Pong: got " ++ x.show ++ ", replying " ++ (x * 10).show)
        send(pong-ch, x * 10)

// ---------------------------------------------------------
// Test: Custom effect - Logger
// Demonstrates that the scheduler is polymorphic over user effects
// ---------------------------------------------------------
effect logger
  fun log(msg : string) : ()

fun test-custom-effect-logger()
  println("\n--- Test: custom effect (logger) ---")
  var log-buffer := []
  val log-ch : chan<int> = channel("log-ch")
  with handler
    fun log(msg) log-buffer := log-buffer ++ [msg]
  schedule fn()
    log("Task 1: starting")
    spawn fn()
      log("Task 2: running")
      send(log-ch, 42)
      log("Task 2: sent message")
    val x = recv(log-ch)
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
  val check-ch : chan<int> = channel("check-ch")
  fun run-scheduler() : <bail-out,console,div> string
    schedule fn()
      println("Task: step 1")
      spawn fn()
        println("Child: checking condition...")
        val x = recv(check-ch)
        if x > 100 then
          println("Child: value too large, bailing out!")
          bail-out("value " ++ x.show ++ " exceeds limit")
        println("Child: value ok")
        shutdown()
      send(check-ch, 999)  // This will trigger bail-out
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
  val start-ch : chan<int> = channel("ch-2")
  schedule fn()
    println("Found Prime: 2")
    spawn( fn(){ filter-actor( start-ch, 2 ) } )
    spawn( fn(){ generator( start-ch, 30 ) } )

// Note: We infer the effect 'e' to be 'console' here because we use println.
fun filter-actor( in-ch : chan<int>, my-prime : int ) : <comm<<console>>,console,div> ()
  val out-ch : chan<int> = channel("ch-" ++ (my-prime + 1).show)
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

fun generator( start-ch : chan<int>, limit : int ) : <comm<<console>>,console,div> ()
  for( 2, limit ) fn(i)
    send(start-ch, i)
    yield() 
  println("Generator finished.")
  shutdown()
