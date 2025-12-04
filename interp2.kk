// ---------------------------------------------------------
// 1. Surface Syntax (The "Sugar")
// ---------------------------------------------------------

type surface
  SLit( i : int )
  SVar( name : string )
  // Generic primitive: op name and list of args
  SPrim( op : string, args : list<surface> ) 
  // Multi-parameter lambda
  SLam( params : list<string>, body : surface )
  // Multi-argument application
  SApp( fnx : surface, args : list<surface> )
  // Let binding: let x = e1 in e2
  SLet( name : string, expr : surface, body : surface )

// ---------------------------------------------------------
// 2. Core Syntax (The "IR")
// ---------------------------------------------------------

type term
  Lit( i : int )
  Var( name : string )
  Prim( op : string, args : list<term> )
  Lam( params : list<string>, body : term )
  App( fnx : term, args : list<term> )

type value
  VInt( i : int )
  VClos( params : list<string>, body : term, captured : list<(string,value)> )

// ---------------------------------------------------------
// 3. The Desugaring Pass
// ---------------------------------------------------------
// Converts Surface -> Core
// Logic: 'let x = v in b'  ==>  '(\x -> b)(v)'

fun desugar( s : surface ) : div term
  match s
    SLit(i) -> Lit(i)
    SVar(n) -> Var(n)
    SPrim(op, args) -> Prim(op, args.map(desugar))
    SLam(ps, b)     -> Lam(ps, desugar(b))
    SApp(f, args)   -> App(desugar(f), args.map(desugar))
    
    // The transformation:
    SLet(name, expr, body) -> 
      App( 
        Lam( [name], desugar(body) ), 
        [desugar(expr)] 
      )

// ---------------------------------------------------------
// 4. Effects
// ---------------------------------------------------------

effect env
  ctl lookup( name : string ) : value
  ctl snapshot() : list<(string,value)>

// ---------------------------------------------------------
// 5. Handlers
// ---------------------------------------------------------

fun run-env( current-env : list<(string,value)>, action : () -> <env,exn|e> a ) : <exn|e> a
  handle(action)
    ctl lookup(name)
      match current-env.find( fn(entry) entry.fst == name )
        Just( (_, v) ) -> resume(v)
        Nothing        -> throw("Runtime Error: Variable '" ++ name ++ "' not found")
    ctl snapshot()
      resume(current-env)

fun run-safe( action : () -> <exn|e> a, ?a/show: a -> e string) : e string
  with handler
    return(x)     x.show
    final ctl throw-exn(msg) "FAILURE: " ++ msg.show
  action()

fun show( v : value ) : string
  match v
    VInt(i) -> i.show
    VClos   -> "<closure>"

// ---------------------------------------------------------
// 6. Primitive Logic
// ---------------------------------------------------------

fun eval-prim( op : string, args : list<value> ) : <exn> value
  match (op, args)
    // Binary Add
    ("+", [VInt(a), VInt(b)]) -> VInt(a + b)
    // Binary Sub
    ("-", [VInt(a), VInt(b)]) -> VInt(a - b)
    // Binary Mult
    ("*", [VInt(a), VInt(b)]) -> VInt(a * b)
    // N-ary Sum (just for fun)
    ("sum", xs) -> 
       val total = xs.foldl(0, fn(acc, v) 
         match v 
           VInt(k) -> acc + k
           _       -> 0 // Ignore non-ints or throw
       )
       VInt(total)
    
    _ -> throw("Primitive error: Unknown op '" ++ op ++ "' or invalid args")

// ---------------------------------------------------------
// 7. The Evaluator
// ---------------------------------------------------------

fun eval( t : term ) : <env,exn,div> value
  match t
    Lit(i) -> VInt(i)
    Var(x) -> lookup(x)
    
    Prim(op, args) ->
      // Evaluate all arguments first (strict evaluation)
      val arg-vals = args.map(eval)
      eval-prim(op, arg-vals)

    Lam(ps, b) -> 
      val cap = snapshot()
      VClos(ps, b, cap)

    App(f, args) ->
      val func-val = eval(f)
      val arg-vals = args.map(eval)
      
      match func-val
        VClos(params, body, saved-env) ->
          if params.length != arg-vals.length then
             throw("Arity Mismatch: Expected " ++ params.length.show ++ " args, got " ++ arg-vals.length.show)
          
          // Zip parameters with values to create bindings
          val bindings = params.zip(arg-vals)
          
          // Extend the captured environment
          val new-env = bindings ++ saved-env
          
          run-env(new-env)
            eval(body)
            
        _ -> throw("Type Error: Application on non-function")

// ---------------------------------------------------------
// 8. Main
// ---------------------------------------------------------

fun interpret( s : surface ) : <div,exn> string
  val core = desugar(s)
  // Run with empty env
  val res = run-env([]) 
    eval(core)
  "Result: " ++ res.show

fun main()
  println("--- Koka Extended Interpreter ---")

  // Test 1: Let Binding + Primitives
  // let x = 10 in x * 2
  val t1 = SLet( "x", SLit(10), 
    SPrim("*", [ SVar("x"), SLit(2) ])
  )
  println( run-safe { interpret(t1) } )

  // Test 2: Multi-param function
  // (\x, y -> x - y) (10, 4)
  val t2 = SApp(
    SLam( ["x", "y"], SPrim("-", [SVar("x"), SVar("y")]) ),
    [ SLit(10), SLit(4) ]
  )
  println( run-safe { interpret(t2) } )

  // Test 3: Nested Scope & Closures
  // let x = 100 in
  //   let f = (\y -> x + y) in
  //     f(5)
  val t3 = SLet("x", SLit(100),
    SLet("f", SLam(["y"], SPrim("+", [SVar("x"), SVar("y")])),
      SApp(SVar("f"), [SLit(5)])
    )
  )
  println( run-safe { interpret(t3) } )

  // Test 4: Generic N-ary primitive
  // sum(1, 2, 3, 4)
  val t4 = SPrim("sum", [SLit(1), SLit(2), SLit(3), SLit(4)])
  println( run-safe { interpret(t4) } )
  println("\n--- Error Handling Tests ---")

  // Error 1: Unbound Variable
  // Querying 'z' which has not been let-bound or passed in.
  val err1 = SVar("z")
  println( "Test 1 (Unbound): " ++ run-safe { interpret(err1) } )

  // Error 2: Type Mismatch in Primitive
  // 10 + (\x -> x) 
  // Trying to add an Integer and a Closure
  val err2 = SPrim("+", [
    SLit(10), 
    SLam(["x"], SVar("x"))
  ])
  println( "Test 2 (Type Error): " ++ run-safe { interpret(err2) } )

  // Error 3: Arity Mismatch
  // Defining a function that takes 2 args (x,y), but calling it with only 1 arg.
  // (\x, y -> x) (10)
  val err3 = SApp(
    SLam(["x", "y"], SVar("x")),
    [ SLit(10) ]
  )
  println( "Test 3 (Arity Error): " ++ run-safe { interpret(err3) } )

  // Error 4: Call Non-Function
  // Trying to use an integer (5) as if it were a function.
  // 5(10)
  val err4 = SApp(
    SLit(5),
    [ SLit(10) ]
  )
  println( "Test 4 (Not a Function): " ++ run-safe { interpret(err4) } )
  
  // Error 5: Unknown Primitive
  // Calling a primitive we haven't defined in 'eval-prim'
  val err5 = SPrim("modulo", [SLit(10), SLit(3)])
  println( "Test 5 (Unknown Prim): " ++ run-safe { interpret(err5) } )