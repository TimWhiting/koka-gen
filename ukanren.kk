// ---------------------------------------------------------
// 1. Data Structures (Terms and State)
// ---------------------------------------------------------

// A Logic Term
type term
  Var( id : int )
  Atom( name : string )
  // A Cons cell for building lists (e.g., [1, 2, 3])
  Pair( head : term, tail : term )
  Unit // Represents '()' or 'end of list'

// The Substitution Map: Logic Variable ID -> Term
// We use a simple association list.
alias subst = list<(int, term)>

// The Solver State
struct state
  next-id : int
  sub : subst

val empty-state = State(0, [])

// ---------------------------------------------------------
// 2. Logic Helpers (Unification)
// ---------------------------------------------------------

// Helper: Walk the substitution chain to find the "real" value of a variable
fun walk( t : term, s : subst ) : div term
  match t
    Var(i) -> 
      match s.find( fn(entry) entry.fst == i )
        Just( (_, v) ) -> walk(v, s) // Keep walking
        Nothing        -> t          // It's a fresh variable
    _ -> t

// Helper: Attempt to unify two terms.
// Returns Just(new-subst) on success, Nothing on failure.
fun unify( t1 : term, t2 : term, s : subst ) : div maybe<subst>
  val u = walk(t1, s)
  val v = walk(t2, s)
  
  if u.eq-term(v) then Just(s) // Already equal
  else match (u, v)
    // If one is a variable, bind it to the other
    (Var(i), _) -> Just( Cons((i, v), s) )
    (_, Var(i)) -> Just( Cons((i, u), s) )
    
    // If both are pairs, recurse on head and tail
    (Pair(h1, t1), Pair(h2, t2)) ->
      match unify(h1, h2, s)
        Nothing -> Nothing
        Just(s2) -> unify(t1, t2, s2)
        
    // Otherwise, mismatch
    _ -> Nothing

// Helper: Structural Equality for terms (needed for 'eq-term' above)
fun eq-term( t1 : term, t2 : term ) : bool
  match (t1, t2)
    (Var(i), Var(j)) -> i == j
    (Atom(a), Atom(b)) -> a == b
    (Unit, Unit) -> True
    // Note: We don't deep check pairs here, unify handles recursion
    _ -> False

// ---------------------------------------------------------
// 3. The Logic Effect
// ---------------------------------------------------------

effect logic
  // Create a new logic variable
  ctl fresh() : term
  
  // Unify two terms. Returns () if success, 'fails' if not.
  ctl unify-term( t1 : term, t2 : term ) : ()
  
  // Non-deterministic choice: returns True or False
  // The handler will execute BOTH paths.
  ctl flip() : bool

// ---------------------------------------------------------
// 4. The Solver Handler (State + List Monad)
// ---------------------------------------------------------

// This handler transforms a 'logic' computation into a function:
// Input:  The current state (s)
// Output: A list of valid resulting states
fun solve( action : () -> <logic,div> a ) : div (state -> div list<(a, state)>)
  handle(action)
    // Return: Wrap the final value and state in a list
    return(x) 
      fn(s) [ (x, s) ]

    // Fresh: Increment ID, return Var, update state
    ctl fresh()
      fn(s) 
        val new-var = Var(s.next-id)
        val new-state = s( next-id = s.next-id + 1 )
        // Resume with the Var, passing new-state to the continuation
        resume(new-var)(new-state)

    // Unify: Try to unify. If fail, return empty list (Stop branch).
    ctl unify-term(t1, t2)
      fn(s)
        match unify(t1, t2, s.sub)
          Nothing -> [] // FAILURE: Prune this branch
          Just(new-sub) -> 
            // SUCCESS: Continue with updated substitution
            resume(())( s(sub = new-sub) )

    // Flip: Branch the universe.
    ctl flip()
      fn(s)
        // Run continuation with True
        val results-true = resume(True)(s)
        // Run continuation with False
        val results-false = resume(False)(s)
        // Combine the results
        results-true ++ results-false

// ---------------------------------------------------------
// 5. User-Facing Syntax (Sugar)
// ---------------------------------------------------------

// Infix operator for unification
fun (===)( t1 : term, t2 : term ) : <logic> ()
  unify-term(t1, t2)

// "conde": Executes a list of clauses. 
// A clause is a unit function. Succeeds if ANY clause succeeds.
fun conde( clauses : list< () -> <logic,div> () > ) : <logic,div> ()
  match clauses
    Nil -> unify-term(Atom("fail"), Atom("fail-me")) // Fail
    Cons(c, rest) ->
      if flip() then 
        c() 
      else 
        conde(rest)

// Helper to convert Koka int lists to Logic Pair lists
fun to-logic-list( xs : list<int> ) : term
  match xs
    Nil -> Unit
    Cons(h, t) -> Pair( Atom(h.show), to-logic-list(t) )

// Helper to reify a term (deep walk and print)
fun reify( t : term, s : subst ) : div string
  match walk(t, s)
    Var(i)   -> "_." ++ i.show
    Atom(n)  -> n
    Unit     -> "[]"
    Pair(h, tail) -> "[" ++ reify(h,s) ++ " . " ++ reify(tail,s) ++ "]"

// ---------------------------------------------------------
// 6. Logic Programs
// ---------------------------------------------------------

// Standard 'appendo' relation in Kanren
// appendo( l, s, out )
fun appendo( l : term, s : term, out : term ) : <div,logic> ()
  conde([
    // Clause 1: l is empty, so s == out
    fn(){ 
      l === Unit
      s === out 
    },
    // Clause 2: l is (h, t), so out is (h, res) where appendo(t, s, res)
    fn(){
      val h = fresh()
      val t = fresh()
      val res = fresh()
      
      l === Pair(h, t)
      out === Pair(h, res)
      appendo(t, s, res)
    }
  ])

fun main()
  println("--- Koka Micro-Kanren ---")

  // PROBLEM: append([1], [2], q)
  // We want to find 'q'.
  println("\nQuery 1: append([1], [2], q)")
  
  val runner1 = solve fn()
    val q = fresh()
    val l1 = to-logic-list([1])
    val l2 = to-logic-list([2])
    
    appendo(l1, l2, q)
    q // We return q to see what it became
  
  val results1 = runner1(empty-state)
  
  results1.foreach( fn(res) 
    val (val-q, final-state) = res
    println( "Solution: q = " ++ reify(val-q, final-state.sub) )
  )

  // PROBLEM: append(x, y, [1, 2, 3])
  // We want to find all combinations of x and y.
  println("\nQuery 2: append(x, y, [1, 2, 3])")
  
  val runner2 = solve fn()
    val x = fresh()
    val y = fresh()
    val target = to-logic-list([1, 2, 3])
    
    appendo(x, y, target)
    (x, y) // Return tuple
  
  val results2 = runner2(empty-state)

  results2.foreach( fn(res)
    val ((val-x, val-y), final-state) = res
    println( "Solution: x = " ++ reify(val-x, final-state.sub) ++ ", y = " ++ reify(val-y, final-state.sub) )
  )