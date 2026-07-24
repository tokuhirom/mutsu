# `Pair.new($k, $v)` value aliases the source scalar's container (PLAN 8.16 [7])

The last open `traps.rakudoc` container-aliasing case landed: raku's `Pair.new`
binds its positional value parameter raw, so the built Pair's value is the
caller's Scalar container itself — a later `$v = 9` shows through the Pair, and
`$p.value = 7` writes through to `$v`. mutsu's method-argument compile path
decontainerizes scalar-variable arguments, so the Pair got a snapshot
(raku `a => 9`, mutsu `a => 1`).

The fix reuses the fat-arrow `key => $var` capture mechanism end to end. The
compiler (`compile_expr_method_on_var`) tags the value argument with
`WrapVarRef` when the call is literally `Pair.new(<expr>, $var)` — bareword
`Pair` receiver, method `new`, exactly two positional args, plain scalar-var
value. The `CallMethodMut` exec then checks the *runtime* receiver: if it is
the native `Pair` type object (and `new` is not user-overridden), the source
local is boxed into a shared `ContainerRef` cell via `capture_var_cell` — the
same call `MakePair` performs; any other receiver (a shadowing user class, a
rebound name) unwraps the tag to the plain value, byte-identical to an
untagged compile.

Scope matches raku exactly: only the 2-positional form aliases. The named form
(`Pair.new(key => "a", value => $x)`), the key argument, and `.freeze` all
decontainerize, and `.value.VAR.^name` reports `Scalar`. Pin:
`t/pair-new-container-alias.t` (verified 8/8 under the reference raku too).

Known shared residue (pre-existing, fat-arrow behaves identically): a
*captured outer* variable read through a closure env has no local slot to box,
so `sub f() { Pair.new("a", $o) }` still snapshots where raku aliases. That is
container-repr territory (ADR-0001), tracked with the §8.13 cross-ref note.
