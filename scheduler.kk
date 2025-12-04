// ---------------------------------------------------------
// 1. Types
// ---------------------------------------------------------

// A task is a function that takes the run-queue and executes.
div type task 
  Task(
    run : (list<task>) -> div ()
  )
// ---------------------------------------------------------
// 2. The Effect
// ---------------------------------------------------------

div effect proc
  ctl yield() : ()
  ctl fork( action : () -> <proc,div> () ) : ()
  ctl exit() : ()

// ---------------------------------------------------------
// 3. The Runtime Implementation
// ---------------------------------------------------------

// Helper: Run the next task in the queue
fun run-next( queue : list<task> ) : div ()
  match queue
    Nil -> () 
    Cons(next, rest) -> next.run.(rest)

// THE FIX: 
// This function transforms a 'proc' action into a 'Task Function' (Queue -> Unit).
// It does NOT run it. It just prepares it.
fun handle-proc( action : () -> <proc,div> () ) : div (list<task> -> div ())
  handle(action)
    
    // 1. Return: Task finishes, run next
    return(_) 
      fn(q) run-next(q)

    // 2. Yield: Add self to back of queue
    ctl yield()
      fn(q)
        // Resume returns a Task Function, so we wrap it in our struct
        val continuation = Task( fn(next-q) resume(())(next-q) )
        run-next( q ++ [continuation] )

    // 3. Fork: Create new task, add to queue, continue self immediately
    ctl fork(child-action)
      fn(q)
        // RECURSION: We use 'handle-proc' to convert the child 
        // into a task function without running it yet.
        val child-task = Task( fn(next-q) { 
           val child-runner = handle-proc(child-action)
           child-runner(next-q)
        })
        
        // Resume self immediately, adding child to queue
        resume(())( q ++ [child-task] )

    // 4. Exit: Drop self, run next
    ctl exit()
      fn(q) run-next(q)


// The Entry Point
fun schedule( main : () -> <proc,div> () ) : div ()
  // 1. Convert main to a task function
  val start-func = handle-proc(main)
  // 2. Run it with an empty queue
  start-func([])

// ---------------------------------------------------------
// 4. Client Code
// ---------------------------------------------------------

fun main()
  trace("--- Koka Green Threads (Corrected) ---")
  
  schedule fn()
    trace("Main: Start")
    
    fork fn()
      trace("  Child A: 1")
      yield()
      trace("  Child A: 2")
    
    fork fn()
      trace("  Child B: 1")
      yield()
      trace("  Child B: 2")

    trace("Main: Yielding...")
    yield()
    trace("Main: Done")