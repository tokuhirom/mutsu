# A MAIN imported inside a block is no longer dispatched

A `MAIN` exported by a module and imported by `{ use Mod; }` stayed registered as
`GLOBAL::MAIN` after the block closed, so the program's implicit MAIN dispatch ran
it at exit. The idiom is how test files load a MAIN-exporting module without running
it, and CLI::Ecosystem's `t/01-basic.rakutest` then ran the whole tool (downloading
and parsing the ecosystem) after printing `ok 1`. Closing an import scope now drops the
`GLOBAL::MAIN` candidates the scope imported, like every other imported routine; a
top-level `use` still makes the module's MAIN the program's.
