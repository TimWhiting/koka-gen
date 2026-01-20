
// ---------------------------------------------------------
// 1. Types (Same as before)
// ---------------------------------------------------------

type term
  Lit( i : int )
  Var( name : string )
  Add( l : term, r : term )
  Lam( param : string, body : term )
  App( fxn : term, arg : term )

type value
  VInt( i : int )
  VClos( param : string, body : term, captured : list<(string,value)> )

// ---------------------------------------------------------
// 2. The Effects (Simplified)
// ---------------------------------------------------------
effect env
  // We only need to ask "what is x?" or "what is the whole env?"
  // We do NOT need 'run-in' here.
  ctl lookup( name : string ) : value
  ctl snapshot() : list<(string,value)>

// ---------------------------------------------------------
// 3. The Handler (The Implementation)
// ---------------------------------------------------------

// This is a standard function that handles the 'env' effect.
// When we need a new scope, we just call this function again.
fun run-env( current-env : list<(string,value)>, action : () -> <env,exn|e> a ) : <exn|e> a
  handle(action)
    ctl lookup(name)
      match current-env.find( fn(entry) entry.fst == name )
        Just( (_, v) ) -> resume(v)
        Nothing        -> throw("Runtime Error: Variable '" ++ name ++ "' not found")
        
    ctl snapshot()
      resume(current-env)

// Error handler remains the same
fun run-safe( action : () -> <exn|e> value ) : e string
  with handler
    return(x)     "Success: " ++ show-value(x)
    final ctl throw-exn(msg) "FAILURE: " ++ msg.show
  action()

fun show-value( v : value ) : string
  match v
    VInt(i) -> i.show
    VClos   -> "<closure>"

// ---------------------------------------------------------
// 4. The Evaluator
// ---------------------------------------------------------

fun eval( t : term ) : <env,exn,div> value
  match t
    Lit(i) -> VInt(i)
    
    // Hits the nearest 'run-env' handler
    Var(x) -> lookup(x) 
    
    Add(l, r) -> 
      match (eval(l), eval(r))
        (VInt(i1), VInt(i2)) -> VInt(i1 + i2)
        _ -> throw("Type Error: Add expects integers")

    Lam(p, b) -> 
      // Hits the nearest 'run-env' handler
      val captured = snapshot() 
      VClos(p, b, captured)

    App(f, a) ->
      val func-val = eval(f)
      val arg-val  = eval(a)
      
      match func-val
        VClos(param, body, saved-env) ->
          val new-env = Cons( (param, arg-val), saved-env )
          
          // KEY CHANGE:
          // Instead of an effect op, we just call the handler function.
          // This pushes a new handler onto the stack for the duration of 'eval(body)'.
          run-env(new-env)
            eval(body)
            
        _ -> throw("Type Error: App expects a function")

fun analyze-interp()
  val ex = App(
    Lam("x", App(
      Lam("y", Add(Var("x"), Var("y"))),
      Lit(20)
    )),
    Lit(10)
  )
  // Initial run with empty environment
  run-safe 
    run-env([]) 
      eval(ex)
fun result-interp()
  "Success: 30"

// ---------------------------------------------------------
// 5. Main
// ---------------------------------------------------------

fun main()
  println("--- Koka Interpreter (Handler Stacking) ---")

  // Example: (\x -> (\y -> x + y) 20) 10
  // Inner function needs to see 'x' from the outer handler 
  // and 'y' from the inner handler.
  val ex = App(
    Lam("x", App(
      Lam("y", Add(Var("x"), Var("y"))),
      Lit(20)
    )),
    Lit(10)
  )

  // Initial run with empty environment
  val result = run-safe 
    run-env([]) 
      eval(ex)
  
  println(result)