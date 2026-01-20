import std/num/random // For the sampling handler
import std/num/float64

// ---------------------------------------------------------
// 1. The Probabilistic Effect
// ---------------------------------------------------------

effect choice
  // Flip a weighted coin. Returns true with probability p.
  ctl flip( p : float64 ) : bool

// A helper for uniform choice among integers [0, n-1]
// We build this out of 'flip' to show composability.
fun uniform( n : int ) : <choice,div> int
  if n <= 0 then 0
  elif n == 1 then 0
  else
    // Split the probability space
    val p = 1.0 / n.float64
    if flip(p) then 0 
    else 1 + uniform(n - 1)

// A semantic alias for 'throw-exn' to represent 
// "This reality is impossible given our observations"
fun reject() : <exn> a
  throw("Observation failed")

// ---------------------------------------------------------
// 2. Handler A: Exact Inference (The Multiverse)
// ---------------------------------------------------------
// This handler explores ALL branches.
// It returns a distribution: a list of (Value, Probability) tuples.

fun exact( action : () -> <choice,exn|e> a ) : e list<(a, float64)>
  with handler
    // 3. Branching path:
    // This is the magic. We resume TWICE.
    // We capture the results of both futures and weight them.
    ctl flip(p)
      // Resume with true (Heads)
      val true-branch : some<a> list<(a, float64)> = resume(True)
      // Resume with false (Tails)
      val false-branch = resume(False)
      
      // Weight the results and merge the universes
      val weighted-true  = true-branch.map( fn(r) (r.fst, r.snd * p) )
      val weighted-false = false-branch.map( fn(r) (r.fst, r.snd * (1.0 - p)) )
      
      weighted-true ++ weighted-false
  with handler 
    // 1. Successful path: Return the value with 100% prob (relative to this leaf)
    return(x) 
      [(x, 1.0)]

    // 2. Failure path: Return empty list (probability mass 0)
    final ctl throw-exn(msg) 
      [] 
  action()

// Helper to pretty-print distributions
fun show-dist( dist : list<(string, float64)> ) : string
  // Combine probabilities for identical outcomes
  // (Naive aggregation for demonstration)
  val total-prob = dist.map(snd).sum
  
  // Normalize (in case of rejections) and Format
  val lines = dist.map fn( d )
    val prob = d.snd / total-prob
    "  " ++ d.fst ++ ": " ++ (prob * 100.0).show(1) ++ "%"
  
  "\nDistribution (Total Mass: " ++ total-prob.show(4) ++ "):\n" ++ lines.join("\n")

// ---------------------------------------------------------
// 3. Handler B: Monte Carlo Sampling (The Simulation)
// ---------------------------------------------------------
// This handler runs a single path using a Random Number Generator.
// It returns Maybe result (Nothing if rejected).

fun sample( action : () -> <choice,random,exn,ndet|e> a ) : <ndet,random|e> maybe<a>
  with handler
    // Use the standard library RNG to pick one path
    ctl flip(p)
      val r = random-float64()
      resume(r < p)
  with handler
    return(x) Just(x)
    
    // If rejected, this single sample failed.
    final ctl throw-exn(msg) Nothing
  action()

// ---------------------------------------------------------
// 4. The Models (Business Logic)
// ---------------------------------------------------------

// Model 1: The "Burglar Alarm" Bayesian Network
// - Burglary happens 1% of time.
// - Earthquake happens 0.2% of time.
// - Alarm triggers if Burglary (95%) or Earthquake (29%).
// - Mary calls if Alarm triggers (70%) or sometimes randomly (1%).
// 
// Query: If Mary Calls, what is the probability there was a Burglary?
fun burglar-model() : <choice,exn> string
  val burglary = flip(0.01)
  val earthquake = flip(0.002)

  val alarm = 
    if burglary && earthquake then flip(0.95) // Both
    elif burglary then flip(0.94)             // Just B
    elif earthquake then flip(0.29)           // Just E
    else flip(0.001)                          // False alarm

  val mary-calls = 
    if alarm then flip(0.70) 
    else flip(0.01)

  // OBSERVATION: We know Mary Called.
  if !mary-calls then reject()

  // We are querying the state of Burglary
  if burglary then "Burglary!" else "Safe"


// Model 2: Drunk Walk (Geometric Distribution)
// How many steps does a drunk person take before falling?
// We limit recursion depth to prevent infinite loops in Exact mode.
fun drunk-walk( steps : int ) : <div,choice> int
  if steps > 10 then steps // Cap at 10 for demo
  else 
    // 50% chance to fall down now
    if flip(0.5) then steps
    else drunk-walk( steps + 1 )

// ---------------------------------------------------------
// 5. Benchmarking
// ---------------------------------------------------------

fun analyze-burglar()
  with pseudo-random(42)
  exact(burglar-model).show-dist

fun result-burglar()
  "\nDistribution (Total Mass: 0.0176):\n" ++
  "  Burglary!: 0.1%\n" ++
  "  Burglary!: 0.0%\n" ++
  "  Burglary!: 37.4%\n" ++
  "  Burglary!: 0.0%\n" ++
  "  Safe: 2.3%\n" ++
  "  Safe: 0.1%\n" ++
  "  Safe: 3.9%\n" ++
  "  Safe: 56.2%"

fun analyze-drunk()
  with pseudo-random(42)
  exact { drunk-walk(0).show }.show-dist

fun result-drunk()
  "\nDistribution (Total Mass: 1.0000):\n" ++
  "  0: 50.0%\n" ++
  "  1: 25.0%\n" ++
  "  2: 12.5%\n" ++
  "  3: 6.2%\n" ++
  "  4: 3.1%\n" ++
  "  5: 1.6%\n" ++
  "  6: 0.8%\n" ++
  "  7: 0.4%\n" ++
  "  8: 0.2%\n" ++
  "  9: 0.1%\n" ++
  "  10: 0.0%\n" ++
  "  11: 0.0%"

fun analyze-burglar-mc()
  with pseudo-random(42)
  var burglary-count := 0
  var total-valid := 0
  for(1, 1000) fn(i)
    val res = sample(burglar-model)
    match res
      Nothing -> ()
      Just(s) -> 
        total-valid := total-valid + 1
        if s == "Burglary!" then burglary-count := burglary-count + 1
  
  val est = burglary-count.float64 / total-valid.float64
  "Total Valid: " ++ total-valid.show ++ ", Prob: " ++ (est * 100.0).show(2) ++ "%"

fun result-burglar-mc()
  "Total Valid: 12, Prob: 41.67%"

// ---------------------------------------------------------
// 6. Main Execution
// ---------------------------------------------------------

fun main()
  println("--- Mini-PPL with Algebraic Effects ---")

  // 1. Run Exact Inference on Burglar Model
  println("\n[Exact Inference] Burglar Model:")
  // 'exact' handles 'choice' and 'exn', returning a list
  val d1 = exact(burglar-model)
  println( d1.show-dist )

  // 2. Run Exact Inference on Drunk Walk
  println("\n[Exact Inference] Drunk Walk (Steps taken):")
  val d2 = exact { drunk-walk(0).show }
  println( d2.show-dist )

  // 3. Run Monte Carlo Sampling (Deterministic with Seed 42)
  println("\n[Monte Carlo] Sampling Burglar Model (1000 runs, Seed 42):")
  
  with pseudo-random(42)
  var burglary-count := 0
  var total-valid := 0
  
  for(1, 1000) fn(i)
    // Run one sample
    val res = sample(burglar-model)
    match res
      Nothing -> () // Rejection
      Just(s) -> 
        total-valid := total-valid + 1
        if s == "Burglary!" then burglary-count := burglary-count + 1
  
  println( "  Total Valid Samples: " ++ total-valid.show )
  if total-valid > 0 then
    val est = burglary-count.float64 / total-valid.float64
    println( "  Estimated Prob of Burglary: " ++ (est * 100.0).show(2) ++ "%" )