# An EVAL's `my class` stays inside that EVAL

A `my class Mixed` declared by an `EVAL` run from a method of a class with a multi-part name
(`A::B::EvS`) stayed reachable from later `EVAL`s in that class. `Mixed.^name`, `my $x = Mixed` and
`::("Mixed")` all found `A::B::EvS::Mixed` again, where rakudo reports `X::Undeclared::Symbols`
(#17750). Rakuish's REPL runs every cell as an `EVAL` in its session class and expects a class from
one cell not to persist into the next.

The cause was two lookups that treated any suppressed short name as a nested type of the running
package. A name is also suppressed when the block or `EVAL` that declared it ends, and then it is
out of scope everywhere. Now only a type a class or role body declares is reached through its owner
package: `resolve_suppressed_type` probes only those, `resolve_type_in_current_package` drops an
out-of-scope lexical storage key, and a type declared while a method of the class runs is no longer
registered as a class-body type.
