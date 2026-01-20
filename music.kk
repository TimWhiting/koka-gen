// ---------------------------------------------------------
// 1. Domain Models
// ---------------------------------------------------------
struct artist
  id : int
  name : string

struct album
  id : int
  artist-id : int
  title : string

struct song
  id : int
  album-id : int
  title : string
  duration : int 

struct library
  next-id : int
  artists : list<artist>
  albums  : list<album>
  songs   : list<song>

val empty-lib = Library(1, [], [], [])

// ---------------------------------------------------------
// 2. The Effects
// ---------------------------------------------------------

// Effect A: The "Repository" Layer (CRUD operations)
effect music-ops
  ctl add-artist( name : string ) : int
  ctl add-album( artist-id : int, title : string ) : int
  ctl add-song( album-id : int, title : string, duration : int ) : int
  ctl get-library() : library

// Effect B: The "Service" Layer (Business Logic)
// This effect is "generic" in that it doesn't care how data is stored,
// it just wants to find things.
effect search
  ctl find-songs( query : string ) : list<song>

// ---------------------------------------------------------
// 3. The Handlers
// ---------------------------------------------------------

// Handler A: Manages the State (The Database)
fun music-manager( initial : library, action : () -> <music-ops|e> a ) : e a
  var db := initial 
  handle(action)
    ctl add-artist(name)
      val id = db.next-id
      db := db( next-id = id + 1, artists = Cons(Artist(id, name), db.artists) )
      resume(id)
    ctl add-album(aid, title)
      val id = db.next-id
      db := db( next-id = id + 1, albums = Cons(Album(id, aid, title), db.albums) )
      resume(id)
    ctl add-song(aid, title, dur)
      val id = db.next-id
      db := db( next-id = id + 1, songs = Cons(Song(id, aid, title, dur), db.songs) )
      resume(id)
    ctl get-library()
      resume(db)

// Handler B: Manages the Search Logic
// Notice the type signature: it handles 'search', but it REQUIRES 'music-ops'
// to exist in the context 'e'. It depends on the lower layer.
fun search-service( action : () -> <search,music-ops|e> a ) : <music-ops|e> a
  handle(action)
    ctl find-songs(query)
      // 1. Ask the lower layer for the data
      val current-db = get-library()
      
      // 2. Perform the logic (case-insensitive substring search)
      val results = current-db.songs.filter fn(s) 
        s.title.to-lower.contains( query.to-lower )
      
      // 3. Return result to the client
      resume(results)

// ---------------------------------------------------------
// 4. Benchmarking
// ---------------------------------------------------------

fun populate-lib() : <music-ops> ()
  val a1 = add-artist("Queen")
  val l1 = add-album(a1, "A Night at the Opera")
  add-song(l1, "Bohemian Rhapsody", 354)
  add-song(l1, "You're My Best Friend", 172)
  add-song(l1, "Love of My Life", 219)

  val a2 = add-artist("The Beatles")
  val l2 = add-album(a2, "Abbey Road")
  add-song(l2, "Come Together", 259)
  add-song(l2, "Here Comes the Sun", 185)
  ()

fun analyze-search-love()
  with music-manager(empty-lib)
  with search-service
  populate-lib()
  find-songs("Love").map(fn(s) s.title).join(", ")

fun result-search-love()
  "Love of My Life"

fun analyze-search-come()
  with music-manager(empty-lib)
  with search-service
  populate-lib()
  find-songs("Come").map(fn(s) s.title).join(", ")

fun result-search-come()
  "Here Comes the Sun, Come Together"

// ---------------------------------------------------------
// 5. Client Code
// ---------------------------------------------------------

fun main()
  // We compose the handlers by nesting the 'with' blocks.
  // 1. The outer handler provides the state (music-ops).
  with music-manager(empty-lib)
  
  // 2. The inner handler provides the search capability, 
  //    consuming 'music-ops' from the outer scope.
  with search-service

  println("--- Populating Library ---")
  
  val a1 = add-artist("Queen")
  val l1 = add-album(a1, "A Night at the Opera")
  add-song(l1, "Bohemian Rhapsody", 354)
  add-song(l1, "You're My Best Friend", 172)
  add-song(l1, "Love of My Life", 219)

  val a2 = add-artist("The Beatles")
  val l2 = add-album(a2, "Abbey Road")
  add-song(l2, "Come Together", 259)
  add-song(l2, "Here Comes the Sun", 185)

  println("--- Testing Search Effect ---")
  
  // Using the new effect
  val query1 = "Love"
  println("Searching for '" ++ query1 ++ "':")
  
  val results1 = find-songs(query1)
  results1.foreach fn(s)
    println("  Found: " ++ s.title)

  val query2 = "Come"
  println("\nSearching for '" ++ query2 ++ "':")
  
  val results2 = find-songs(query2)
  results2.foreach fn(s)
    println("  Found: " ++ s.title)