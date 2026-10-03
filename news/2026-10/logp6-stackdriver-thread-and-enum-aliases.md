# LogP6::Writer::StackDriver passes: `$*THREAD` identity, enum short-name aliases, `$?NL`

LogP6::Writer::StackDriver's test suite now passes on mutsu. It needed four
interpreter fixes:

- **`$*THREAD` is one object per thread.** Each read used to build a fresh
  `Thread` instance, so LogP6's `$*THREAD does LogP6::ThreadLocal` mixed its
  per-thread context into a throwaway. The object is now cached per OS
  thread. Inside `Thread.start`, `$*THREAD` is the very `Thread` object that
  was returned. `Thread.finish` no longer holds the receiver's attribute read
  lock while it joins, which deadlocked once the joined thread mixed a role
  into its own `$*THREAD`.
- **Qualified enum exports keep their short name.** `enum A::Level is export`
  now exports `Level` as well. A module routine can use `Level::error` and
  `Level($n)` through the short alias its module's `use` installed, even when
  the importer took only a tagged export (`use LogP6 :configure`).
- **`$?NL`** is provided ("\n" by default), and `use newline :crlf/:cr/:lf`
  changes it for the enclosing block.

Follow-ups filed: #11455 (a module-scope `atomicint` read from an imported
routine) and #11456 (a mixin type created on another thread is not visible to
smartmatch).
