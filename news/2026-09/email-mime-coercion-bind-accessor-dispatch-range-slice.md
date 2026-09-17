# A coercion-typed `:=` rebind, `callwith` reaching an ancestor accessor, Range slicing with a Rat endpoint, and `.encode(:!replacement)`

Working the `Email::MIME` zef distribution (locked via #7884) turned up four
separate, general interpreter bugs, all found through the same distribution's
own test suite but unrelated to each other in the code.

## A `:=` rebind re-applied a stale coercion-type constraint

`Email::Simple.create(Array() :$header is copy, ...)` does
`$header := $header-class.new($header, ...)` partway through, replacing the
coerced `Array` it started with with a freshly-built header object. mutsu's
scalar-store path registers a parameter's coercion type (`Array()`) as a
name-keyed constraint used to re-coerce every later STORE to that name — but
it did not distinguish a `:=` bind (which should alias the RHS verbatim,
never coercing) from a plain `=` assignment (which legitimately keeps
coercing on every store). So the rebind above silently ran the `Array()`
coercer against the new header OBJECT, turning `$header` back into an
`Array` and breaking every method call on it
(`No such method 'header' for invocant of type 'Array'`). A genuine
*declared* type (`my Array $x`, no coercion parens) is unaffected — Raku
still type-checks a bind against a real declared type, it just never
coerces; only a coercion-type constraint is exempt from bind-time
re-application. Fixed in `exec_set_local_op_inner`
(`src/vm/vm_var_assign_set_local.rs`): a scalar or container bind whose
stored constraint is a coercion type is now treated as unconstrained.

## `callwith`/`callsame`/`nextsame` couldn't reach an ancestor's auto-generated accessor

`Email::MIME.body($decorate?)` overrides the inherited `$.body` reader and
calls `callwith()` to read the raw stored value before decoding it — a
completely ordinary "wrap the accessor" pattern. Auto-generated attribute
accessors are registry metadata, not `MethodDef`s, so they never appeared
among the dispatch candidates a single-user-candidate call collects, and
`push_method_dispatch_frame` skipped pushing an MRO deferral frame entirely
whenever a method had exactly one *declared* candidate — even when an
ancestor's accessor was a legitimate, un-collected second candidate.
`callwith`/`callsame`/`nextsame` then had nothing to defer to and silently
answered `Nil` instead of the attribute's value. `push_method_dispatch_frame`
(`src/runtime/accessors_state.rs`) now also checks whether a shadowed
ancestor declares a public accessor of the same name and, if so, pushes a
frame whose terminal candidate is that accessor — the same `DeferralEntry::Accessor`
mechanism a *wrapped* accessor already used, just reachable now from an
ordinary overriding method too.

## A Range subscript with a `Rat` endpoint silently emptied the slice

Dividing two `Int`s in Raku always produces a `Rat`, even on an even split
(`2/2` is `Rat` `1.0`, not `Int` `1`) — so `^($x.codes / 2)`, the pattern
`Email::Simple`'s header-folding preprocessor uses to compute its CRLF/LF
newline style, builds a `Range` whose endpoint the `Array`/`Uni` positional
subscript's endpoint resolver did not recognize. It fell through a `_ => 0`
catch-all and returned an empty (or, for `Uni`, entirely unsliced) result
instead of resolving the `Rat`. Fixed with a shared `generic_range_bound`
helper (`src/vm/vm_var_index_ops.rs`) that resolves a `Rat`/`FatRat`/`Num`
endpoint to the correct integer subscript boundary — matching Raku's own
Range-iteration semantics for a genuinely fractional endpoint too (`@a[^2.5]`
includes index 2 as well as 0 and 1, not just the two below its floor), not
just the whole-valued case `Email::MIME` happens to hit.

## `.encode('ascii', :!replacement)` didn't throw

`Str.encode`'s named `:replacement` argument decides between a lenient
encode (substituting a replacement string/char for an unencodable codepoint)
and the default strict one (which throws). An explicit `:!replacement` -- a
`Pair` whose value is `False` -- was treated exactly like any other *present*
named argument: unconditionally stringified and used as the literal
replacement text, so `.encode('ascii', :!replacement)` silently substituted
bytes from the string `"False"` for every unencodable codepoint instead of
throwing `X::AdHoc: Error encoding ASCII string: ...`. `Email::MIME`'s
RFC 2231 filename-encoding logic (`try { $value.encode('ascii', :!replacement) }`,
branching on whether `$!` got set) depends on that exception to decide
whether a `Content-Disposition` filename needs percent-encoding. Fixed in
`dispatch_encode` (`src/runtime/methods_io_dispatch.rs`): `Bool(false)` now
maps to `None` (the strict path), same as omitting the adverb entirely.

## Residue, filed rather than fixed here

Two further gaps surfaced by the same distribution are architecturally
deeper and filed separately: `nextwith`/`callwith` called with FEWER
arguments than the original call can wrongly exclude an ancestor candidate
whose own arity only matches the deferred call, not the original one
(#8654); and `for $scalar.list -> $v is rw { $v = ... }` does not write the
loop body's mutation back to the source scalar the way the sibling
`for @a[i] { $_ = ... }` element-writeback case already does (#8655).
`Email::MIME` itself moves from `partial` (3/6 baseline files) towards
`partial` with more of its assertions passing; the remaining two baseline
files are explained by those two issues.

Regression coverage: `t/routines/signature/coercion-param-scalar-rebind-no-recoerce.t`,
`t/oo/attribute/callwith-reaches-ancestor-accessor.t`,
`t/collections/range-pair/range-slice-rat-endpoint.t`,
`t/types/string/str-encode-strict-adverb-false.t`.
