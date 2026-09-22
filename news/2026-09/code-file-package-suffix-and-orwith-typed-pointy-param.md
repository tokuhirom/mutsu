# `Code.file` module-package suffix, and a typed-pointy-param `orwith` parser bug

Working `Code::Coverage` 0.0.8 from the ecosystem ledger (locked on
[#8977](https://github.com/tokuhirom/mutsu/issues/8977)) surfaced two unrelated, general-purpose
interpreter bugs on the way to its `t/01-basic.rakutest`, both now fixed and pinned.

## `Code.file`/`Code.line` was missing the compunit's own package suffix

Real Raku's `.file` on a routine declared inside a real compilation unit (a `use`d/`require`d
module) is suffixed with `" (Package::Name)"` — the identity the *compilation unit itself* declares
(via `unit module X;`, or, when a file has no such header, the name it was `use`d under), not the
specific lexical package the routine happens to sit in. A method on a class nested inside a module
still reports the *module's* name in that suffix, never the class's. mutsu returned the bare file
path with no suffix at all, for every routine shape (subs, methods, grammar tokens/rules).

This is not cosmetic: `Identity::Utils`'s ecosystem-wide "did this sub come from my own compunit"
idiom (used by `Code::Coverable`'s custom `EXPORT(*@names)`, which every distribution in
`Code::Coverage`'s dependency chain relies on) is exactly `&code.file.ends-with("($module)")`,
filtering `UNIT::{"&$name"}` symbol-table lookups down to symbols actually declared in the current
file. Without the suffix, every such check silently returned `Nil`, and the module exported
nothing — `Code::Coverage`'s test died with `Unknown function: bytecode` before ever reaching its
own logic.

Fixed by reusing `Interpreter::lexical_package_for_frame` (already used for backtrace-frame
attribution) to look up a routine's declaring compunit's own package identity from its
`source_file`, and appending it to `.file`'s answer in the three places that build one: a `Sub`
value's own `.line`/`.file` dispatch, a by-name `Routine` handle's, and a `Method`/grammar-token
`Instance`'s. Pinned by `t/modules/code-file-module-package-suffix.t`.

## `with EXPR -> TYPE $param { ... } orwith ... -> TYPE $param2 { ... }` failed to parse

`parse_elsif_chain`'s hand-rolled `orwith` pointy-parameter scan only recognized a bare sigil
(`$`/`&`/`@`/`%`) immediately after `->`; a type name in front of it (`-> int $c`) was never
consumed, so the following `block()` call failed on the dangling `-> int $c { ... }` text. That
parse failure propagated all the way up through `with_stmt`'s delegation to the shared chain
parser, so the *whole* `with`/`orwith` chain failed and fell back to being re-parsed as a bareword
call — `Undeclared routine: orwith used`, or, when the leading `with` clause was *also*
typed-pointy, the even more confusing `Unexpected block in infix position` on the first `with`'s
own opening brace.

This is `Identity::Utils`'s `short-name` helper verbatim:

```raku
with $identity.rindex('::') -> int $offset { ... }
orwith $identity.index(':') -> int $chars { ... }
else { ... }
```

Fixed by reusing the same type-constraint consumption `parse_for_pointy_param` already does,
before the sigil scan. Pinned by `t/routines/signature/with-orwith-typed-pointy-param.t`.

## What's left for `Code::Coverage`

The distribution's `t/01-basic.rakutest` now reaches further before dying — into
`CompUnit::Repository::FileSystem.resolve`, which has no implementation in mutsu at all (and the
one existing sibling implementation, `CompUnit::Repository::Installation.resolve`, is a `Bool`
stub rather than a real `CompUnit::Handle`). That is deep CompUnit-Repository/precompilation
machinery, filed as [#9004](https://github.com/tokuhirom/mutsu/issues/9004) rather than forced
through here.
