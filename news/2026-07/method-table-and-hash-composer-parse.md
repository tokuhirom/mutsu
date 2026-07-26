# `.^method_table`, the hash-composer rule, and the bare-adverb listop parse

Fixes taken off the `DBIish` blocker ledger
(`todo/tickets/dbiish-blockers.md`), all of them general-purpose. Together they
take `DBIish`'s `t/01-basic.rakutest` from dying before its first test to running
18 of 35, and `t/05-mock.rakutest` from aborting after 13 of 16 tests to running
all 16 with one failure left (a separate iterator bug, still on the ledger).

## `.^method_table`

`Metamodel::ClassHOW.method_table` returns the methods declared directly on a
class, keyed by name. mutsu implemented the sibling `submethod_table` but not
this one, so `DBIish.^method_table{$_}:exists` died with `No such method
'method_table'`. It was recorded in the ledger as a `PackageHOW` gap; reducing it
to a repro showed a plain `ClassHOW` has the same hole.

Rakudo's table holds exactly the class's *own* methods: inherited methods are
not in it, submethods live in `.^submethod_table`, private methods in
`.^private_method_table`, while role-composed methods and public attribute
accessors are included and a `multi` contributes a single dispatcher entry.
`class_method_table` in `runtime/methods_classhow_method_obj.rs` builds that, and
the values are real `Method` objects rather than name strings. Pinned by
`t/method-table.t`, which passes unchanged under rakudo.

## `{ ... }` is a hash composer when its first element *is* a pair

`{ +(SQLT_CHR) => Str, … }` — a constant hash whose keys are expressions — parsed
as a block, so `DBDish::Oracle::Native` died with `Odd number of elements found
where hash initializer expected`. mutsu decided hash-versus-block from a
syntactic prefix table: a bareword, number or quoted string followed by `=>`, a
`%` variable, or a colonpair. Any other key expression fell through to the block
reading.

Rakudo's rule is about the *first element of the first statement*: the braces
compose a hash when that element's outermost operator is a fatarrow. A cheap
scan now decides whether a top-level `=>` is even in reach — it stops at a
statement keyword, an assignment (`{ my %h = a => 1 }` assigns a pair, it is not
one), a listop colon call, a ternary, or a preceding top-level comma or
semicolon — and when it is, the first element is parsed and the fatarrow has to
come out as the root of the resulting expression. That last step is what keeps
`{ group-of 1 => 'x' }` a block: the scan sees the arrow, but the parse says the
root is a listop call whose argument happens to be a pair.

Widening the detection exposed one bug behind it: the hash-literal body parser
committed to a bare leading key whenever what followed was not `=>`, so
`{ 'a' ~ 'b' => 1 }` became the key `a` followed by `~ 'b' => 1` and produced
`{:a(:b(1))}`. A bare key now stands alone only when the element ends right
there (`{ a, 1 }`); anything else falls through to the ordinary expression
parse, which already had the precedence right.

An invocant-less method call also counts as a topic reference now, which is what
makes `{ .key => 1 }` and `{ a => .key }` blocks in rakudo. mutsu already
excluded an explicit `$_`; the leading-dot form was missed, and with the widened
hash detection above it would have turned `map({ +SQLType::{.key} => .value },
…)` — real code in `DBIish::Common` — into a hash. A `.^name` inside a string
interpolation still does not count, since it belongs to that interpolation's own
closure.

## A bare adverb no longer swallows the enclosing argument list

`is-deeply $sth.row :hash, %want, 'desc'` must call `.row(:hash)` and pass the
remaining two arguments to `is-deeply`. mutsu handed all three to `.row`, so a
zero-positional signature reported `Too many positionals passed; expected 0
arguments but got 2`.

The method-call parser distinguishes `.m: a, b` (colon call, takes the comma
list) from `.m :adv` (space before the colon, an adverb) when it reads the first
argument, but both then shared the same continuation loop, and that loop kept
consuming `, next`. The adverb form now continues with further space-separated
adverbs only — `$h.row :hash :other` binds both, `$h.row :hash, :other` binds
only the first and leaves `:other` to the enclosing call, matching rakudo.

## `$*VM.config<nativecall_backend>`

`NativeLibs` reads `$*VM.config<nativecall_backend> eq 'dyncall'` unconditionally,
and mutsu's two-key config warned `Use of uninitialized value of type Any in
string context` on every load — noise that had twice been mistaken for a
diagnosis while working through this ledger. mutsu's FFI is libffi, which is
also what a modern MoarVM reports, so the key now says so.
