// ---------------------------------------------------------
// 1. Types
// ---------------------------------------------------------

// A task is a prepared computation that runs in the scheduler loop.
// It has 'div' (loops) and 'local' (updates scheduler state).
value struct task
  run : some<h> () -> <console,div,local<h>> ()

// A waiter is a captured continuation expecting an int message.
// It returns the next task function.
value struct waiter
  callback : some<h> (int) -> <console,div,local<h>> (() -> <console,div,local<h>> ())

// The communication effect.
// We use 'div effect' to allow recursion.
div effect comm
  ctl spawn( action : () -> <comm,console,div> () ) : ()
  ctl yield() : ()
  ctl send( chan : string, v : int ) : ()
  ctl recv( chan : string ) : int
  ctl shutdown() : ()

// ---------------------------------------------------------
// 2. The Scheduler
// ---------------------------------------------------------

fun schedule( main : () -> <comm,console,div> () ) : <console,div> ()
  
  // --- Runtime State ---
  var ready : list<task> := []
  var mailboxes : list<(string, list<int>)> := []
  var waiting : list<(string, list<waiter>)> := []

  // --- Helpers ---
  fun enqueue( t : task )
    ready := ready ++ [t]

  fun find-map( l : list<(string, a)>, key : string ) : maybe<a>
    match l.find( fn(p) p.fst == key )
      Just((_, v)) -> Just(v)
      Nothing      -> Nothing

  // --- The Run Loop ---
  fun run-loop() : <console,div,local<_>> ()
    match ready
      Nil -> () 
      Cons(current-task, rest) -> 
        ready := rest
        current-task.run.()

  // --- The Handler ---
  // Converts a <comm> action into a task function () -> <div,local> ()
  fun handle-comm( action : () -> <comm,console,div> () ) : <console,div,local<_>> (() -> <console,div,local<_>> ())
    handle(action)
      return(_) 
        fn(){ run-loop() }

      ctl shutdown()
        ready := []
        fn(){ () }

      ctl yield()
        val k = resume(()) // k is () -> (() -> ...)
        enqueue( Task(k) )
        fn(){ run-loop() }

      ctl spawn(child)
        // handle-comm(child) returns the function that starts the child.
        val child-task = handle-comm(child)
        enqueue( Task(child-task) )
        resume(())

      ctl send(chan, msg)
        val waiters = waiting.find-map(chan).default([])
        match waiters
          Cons(w, w-rest) ->
            waiting := waiting.filter(fn(p) p.fst != chan) ++ [(chan, w-rest)]
            // w.callback(msg) returns the next task function for the waiter
            val waiter-task = w.callback.(msg)
            enqueue( Task(waiter-task) )
            resume(())
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
            fn(){ run-loop() }

  // --- Bootstrap ---
  val main-task = handle-comm(main)
  enqueue( Task(main-task) )
  run-loop()

// ---------------------------------------------------------
// 3. Application Code
// ---------------------------------------------------------

fun filter-actor( in-ch : string, my-prime : int ) : <comm,console,div> ()
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

fun generator( start-ch : string, limit : int ) : <comm,console,div> ()
  for( 2, limit ) fn(i)
    send(start-ch, i)
    yield() 
  println("Generator finished.")
  shutdown()

fun main()
  println("--- Concurrent Prime Sieve (Final) ---")
  schedule fn()
    val start-ch = "ch-2"
    println("Found Prime: 2")
    spawn( fn(){ filter-actor( start-ch, 2 ) } )
    spawn( fn(){ generator( start-ch, 30 ) } )
