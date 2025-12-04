import std/os/process
import std/os/file
import std/os/path
import std/os/env
import std/core/console
import std/core/debug

fun exit(code: int): io ()
  impossible(code.show)

// Helper to run a command and verify success
fun run-checked( cmd : string, error-msg : string ) :io ()
  val exit-code = run-system(cmd)
  if exit-code != 0 then
    println("Error: " ++ error-msg)
    exit(1)

// Helper to run a command and capture its trimmed stdout
// Uses a temporary file strategy for maximum reliability with standard Koka libraries
fun run-capture( cmd : string ) :io string
  val tmp = "out/.koka-git-out.tmp"
  // Run command and redirect stdout to temp file
  val full-cmd = cmd ++ " > " ++ tmp
  val exit-code = run-system(full-cmd)
  
  if exit-code != 0 then
    // Cleanup and throw/exit on failure
    // try-default((), fn() remove-file(path(tmp))) 
    "" // Return empty to signal failure/empty
  else 
    val content = read-text-file(path(tmp))
    // remove-file(path(tmp))
    content.trim()

fun main() 
  println("Checking repository status...")

  // 1. Fail if there are uncommitted changes
  // --porcelain guarantees stable output format. If it returns anything, the repo is dirty.
  val status = run-capture("git status --porcelain")
  if status != "" then
    println("FAILURE: Uncommitted changes detected.")
    println("Please commit or stash your changes before running this script.")
    exit(1)

  // 2. Get the latest tag
  // --abbrev=0 gets the tag name only, not the long format
  val latest-tag-str = run-capture("git describe --tags --abbrev=0")

  if latest-tag-str == "" then
    println("No existing tags found. Please create an initial tag (e.g., v0.0.0) manually.")
    exit(1)

  // 3. Check if there are commits since the latest tag
  // "HEAD" compares current state against the tag reference
  val diff-check = run-capture("git rev-list HEAD ^" ++ latest-tag-str)
  
  if diff-check == "" then
    println("No changes detected since " ++ latest-tag-str ++ ". No new tag needed.")
    exit(0)

  // 4. Parse and Increment Semver
  // Expected format: vX.Y.Z
  if !latest-tag-str.starts-with("v").bool then
    println("Error: Latest tag '" ++ latest-tag-str ++ "' does not start with 'v'.")
    exit(1)

  val version-part = latest-tag-str.slice.drop(1).string // remove 'v'
  val parts = version-part.split(".")

  match parts
    Cons(major, Cons(minor, Cons(patch, Nil))) -> 
      match (parse-int(major), parse-int(minor), parse-int(patch))
        (Just(maj), Just(min), Just(pat)) ->
          // Increment PATCH version for end-of-day updates
          val new-patch = pat + 1
          val new-tag = "v" ++ maj.show ++ "." ++ min.show ++ "." ++ new-patch.show
          
          println("Changes detected. Incrementing version: " ++ latest-tag-str ++ " -> " ++ new-tag)
          
          // 5. Create the new annotated tag
          val tag-msg = "Daily release " ++ new-tag
          run-checked("git tag -a " ++ new-tag ++ " -m \"" ++ tag-msg ++ "\"", "Failed to create git tag.")
          println("Successfully created tag: " ++ new-tag)
        
        _ -> 
          println("Error: Could not parse integers from version string " ++ version-part)
          exit(1)
    _ -> 
      println("Error: Tag format " ++ latest-tag-str ++ " does not match vX.Y.Z")
      exit(1)