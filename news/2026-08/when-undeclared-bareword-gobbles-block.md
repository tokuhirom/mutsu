# `when SomeUndeclaredType { … }` is a parse-time `X::Comp::Group` for any name

`given 42 { when SomeUndeclaredType { 1 }; default { 0 } }` is a compile-time
error in raku: an undeclared bareword immediately followed by a block is read as
a routine call that gobbles the block, leaving `when` without the block it
requires, so the parser raises an `X::Comp::Group` bundling an
`X::Syntax::BlockGobbled` sorrow and an `X::Syntax::Missing` panic. mutsu ran the
program to completion instead, quietly falling through to `default`.

mutsu already had this diagnosis in `when_stmt`
(`src/parser/stmt/control/given_when.rs`), but deliberately restricted to the
reserved `X::` / `CX::` exception namespaces. `todo/deep/` recorded the
broadening as blocked on a cross-file type index: types declared in a sibling
file of the same distribution and pulled in with `use` are registered by mutsu at
*run* time, so a parse-time "this name is declared nowhere" check would reject
valid code.

## The index already existed

That premise was stale. `register_module_exports`
(`src/parser/stmt/simple/module_exports.rs`) has since grown into exactly the
index the ticket asked for: on every `use`/`need`/`require` the parser resolves
the module to a file, scan-parses it, and harvests the class/role/grammar/enum
names it declares — transitively through the modules *it* uses, memoized per
resolved path. The ticket's own counterexample,
`Cro::HTTP::ResponseParser`'s `when Header { … }`, resolves correctly today.

Measuring the naive broadening against the vendored corpus (every file under
`t/`, `roast/`, `modules/`, `vendor/zef/` containing a `when <bareword> {`) left
only four precise, fixable gaps rather than an architectural one. Note that
`--dump-ast` is useless for this measurement: it runs before the interpreter
installs the module search paths, so the parse-time scan resolves nothing and
every imported type looks undeclared. The survey has to run the files.

## What was fixed

* **`unit` declarators lost their package prefix in the module scan.**
  `unit module Foo;` parses to a declaration with an empty body followed by its
  contents as *siblings*, so `collect_module_type_names_under` never descended
  into it. A nested `class Part` under `unit module Cro::HTTP::Body;` was only
  ever harvested as `MultiPartFormData::Part`, never as the
  `Cro::HTTP::Body::MultiPartFormData::Part` spelling importers write. The
  harvest now carries a unit declarator's composed name forward across the rest
  of the statement list.
* **An `enum`'s own name was never registered as a type.** `enum Day <Mon Tue>`
  registered its *values* but not `Day`, so both `when Day { … }` and the
  package-qualified value spelling `when Day::Mon { … }` looked undeclared.
* **Type smileys.** `when Map:D { }` / `when Channel:U { }` arrive as a bareword
  carrying the smiley. A smiley can only attach to a type name, so such a
  matcher is never a routine call gobbling a block.
* **Package-qualified enum values.** `when Day::Mon { }`,
  `when HTTP::HPACK::Indexing::Indexed { }` — recognized when the head names a
  declared enum type and the last segment one of its values.
* **EVAL saw no user types.** The nested parse starts from an empty scope stack
  and mutsu keeps user types in the runtime registry, so
  `class Foo {}; EVAL 'given Foo { when Foo { … } }'` would have been rejected.
  A new `set_eval_user_type_preseed`, populated from the registry's
  classes/roles/enums/subsets, mirrors the existing sub/operator preseeds.

## The honest completeness gate

One hole cannot be closed by scanning harder: a module the parser cannot resolve
to a file. `find_module_file` skips `inst#` entries (mzef-installed
repositories), a runtime `require ::($name)` names its module only at run time,
and a module simply absent from the environment resolves to nothing. In all
three cases the unit can legally see types the parse-time index does not know.

So the index now reports its own completeness. `note_type_index_incomplete` is
called when a non-pragma import fails to resolve and scan, when a `require`
names its target dynamically or by path, and — replayed through
`ModuleScanResult`, so a scan-cache hit propagates it too — when a scanned
dependency's own index was incomplete. While the flag is set, the broadened
diagnosis stands down and `when_stmt` falls back to the old `X::`/`CX::`-only
rule, whose members are either known builtin exceptions or genuinely undeclared.
The flag is cleared per compilation unit in `reset_user_subs`.

The result is a check that only ever claims "declared nowhere" when it actually
has a complete view. Across the 119 corpus files carrying a bareword `when`
matcher there are zero false positives, and both `t/` (3336 files) and the full
roast whitelist stay green.

Pinned by `t/when-undeclared-type-gobbles-block.t`: the error case (plain and
package-qualified undeclared names) plus the must-still-parse cases — a type
declared earlier in the same file, a nested type and a role imported from a
module under a `unit module`, a locally declared enum type and its qualified
value, a type smiley, and an EVAL that sees its caller's types.

Note that "declared later in the same file" is *not* a must-still-parse case:
raku is one-pass here too, and `given 42 { when Foo {} }; class Foo {}` is the
same `X::Comp::Group`.
