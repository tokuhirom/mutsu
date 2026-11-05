# A callee's `EVAL` no longer reads its caller's block-scoped `sub`

`sub callee { EVAL q[&helper] }` called from `sub outer { my sub helper { 'H' }; callee() }` returned
the caller's `helper` (and `EVAL q[&helper()]` called it); `raku` reports it as not visible. The code
object a routine's body keeps for its own `sub`/`my sub` sits under a reserved env key that no
static-link rule covered, so a reflective lookup walked past the callee's frame root into the
caller's frame. That key is now a lexical like `&name` itself: a top-level routine's lookup stops at
its own scope, and a closure still finds the sub through its capture.
