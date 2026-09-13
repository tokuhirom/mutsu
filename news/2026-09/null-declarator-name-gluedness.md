# A bare `::` is now the null routine/method name, not a misread indirect name

`UI::HTMLWindow`'s `lib/UI/HTMLWindow.rakumod` (found while working the
parse-failure index, #7954) calls `&routine.wrap(anon method :: (Window:
*@_, *%_) { ... })`. mutsu could not parse it: `sub`/`method` declarator-name
parsing required an identifier first, so a leading `::` fell through to the
`::(EXPR)` *indirect declarator name* form unconditionally, regardless of
whether the `::` was glued to the following `(` or separated from it by a
space.

Verified against rakudo: gluedness is what disambiguates the two forms.
`sub ::(name) (...) {...}` (glued) declares a sub using the *value* `name`
evaluates to as its actual name — mutsu's own, more permissive take on
rakudo's indirect-declarator-name feature (rakudo additionally requires the
name expression to be compile-time-known; mutsu evaluates it at declaration
time instead, which the existing `t/vm/binding/indirect-declarator-names.t`
already exercises and pins). `sub :: (...) {...}` (a space before the paren)
is unrelated: `::` alone is the *null routine name* — "this routine has no
name" — and `(...)` is an ordinary signature, exactly like the pre-existing
`sub (...) {...}` / `anon method (...) {...}` forms.

Before this fix, the space form was silently misparsed: `anon sub :: ($x) {
42 }` read `($x)` as the indirect name's expression instead of a signature,
leaving the routine both unnamed *and* unsigned (calling it raised "No such
method 'CALL-ME'" rather than running the body). `anon method :: (Window:
*@_, *%_) { ... }` failed to parse at all.

The fix is a gluedness check (`strip_null_decl_name_marker`,
`src/parser/primary/ident/identifier_call.rs`) applied at every site that
already treats a bare `sub`/`method` (no name, no `::`) as anonymous —
`anon sub`, `anon method`, and the bare `sub`/`method` expression-position
terms — plus tightening `parse_indirect_decl_name` itself
(`src/parser/stmt/sub/sub_decl.rs`) to require the glued form, so the
existing statement-level declarators (`sub_decl_body`,
`method_decl_body_with_my`) now correctly fail and fall through to the
already-nameless path on a spaced `::`, rather than misreading it.

Pinned by `t/vm/binding/declarator-null-name.t`, verified against `raku`.

Filed while working this: [#8311](https://github.com/tokuhirom/mutsu/issues/8311)
— `anon method (Invocant: *@a) {...}` compiles as a plain `Sub`/`Block`
rather than a `Method`, and does not exclude the invocant from the slurpy.
That divergence predates this ticket (reproduces identically with and
without `::`, with and without `anon`) and is out of scope here.

[#8294](https://github.com/tokuhirom/mutsu/issues/8294)
