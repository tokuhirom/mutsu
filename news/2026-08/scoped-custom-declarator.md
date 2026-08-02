# A custom declarator accepts a `my`/`our` scope prefix

A declarator registered through a module's `EXPORTHOW::DECLARE` block — the
`monitor` keyword `OO::Monitors` installs, for instance — parsed only in its
bare form. Writing `my monitor Counter { ... }` died at parse time with
`X::Syntax::Malformed: Malformed my (did you mean to declare a sigilless \var
or $var?)`.

The two paths had drifted apart. `class::declare_decl` walks the registered
keyword table (`declare_keyword_names()`) and hands the match to
`class_decl_body`, tagging the result with the `__mutsu_declare_how` marker
trait so registration can attach the declarator's HOW. The scope-prefixed path,
`decl::my_decl_dispatch::try_keyword_dispatch`, was a hardcoded if-chain of
built-in keywords (`class`, `grammar`, `role`, `module`, `package`, `subset`,
`constant`, `regex`/`token`/`rule`) that had never heard of the registry. Worse,
`decl::my_decl` is dispatched *before* `class::declare_decl` in `STMT_PARSERS`
and fails fatally, so there was no fall-through: an unknown keyword after `my`
went straight to the sigil check and raised the malformed-declaration error.

`try_keyword_dispatch` now consults `declare_keyword_names()` as its last
resort, after every built-in keyword, so a module cannot shadow `class` or
`role` by registering a declarator with one of those names. The match is parsed
with `class_decl_body(rest, !is_our)` — lexical for `my`, package-scoped for
`our` — and carries the same `__mutsu_declare_how` trait as the bare form.

This was the load barrier for `Cro::HTTP::Client`, which declares
`my monitor ConnectionCache { ... }`: the module failed to parse, and with it
five of the upstream suite's test files (`http-auth-basic`,
`http-auth-basic-with-session`, `http-middleware`, `http-session-inmemory`,
`http-session-persistent`). All five now load and reach real Cro runtime code.

Pinned by `t/scoped-custom-declarator.t`, which passes identically under raku.
