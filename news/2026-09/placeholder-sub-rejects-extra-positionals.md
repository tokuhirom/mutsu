# A placeholder sub rejects extra positionals, and `%_` never bought the leniency

`sub f { $^a + $^b }` called with three arguments silently succeeded. Rakudo
refuses it:

```
sub direct { $^a + $^b }
direct(23, 1, 4)
    # raku:  Too many positionals passed; expected 2 arguments but got 3
    # mutsu: silently succeeds
```

The **block** spelling of the same signature was already correct, and `.arity`
and `.count` both answered `2`; only the surplus went unchecked, and only for a
routine whose parameters come from `^`-twigil placeholders.

## The exclusion was real but three times too wide

`bind_function_args_values`'s legacy branch gated its "too many" check on
`all_plain_positional` — a `params` list with no `^`/`:` placeholder — and
`news/2026-08/fast-binder-too-many-positionals-check.md` recorded why: a
placeholder sub "whose body also reads a bare `@_`/`%_` legitimately accepts
more positionals than its placeholders declare".

Measured against rakudo, that is half true. Four placeholder subs called as
`f(1, 2, 3)`:

| sub | raku |
| --- | --- |
| `sub a { $^x }` | **dies**, "expected 1 argument but got 3" |
| `sub b { $^x; @_.elems }` | 2 — the surplus really is `@_` |
| `sub c { @_.elems }` | 3 |
| `sub d { $^x; %_.elems }` | **dies** |

Row `d` is the correction: a `%_` read is about *named* arguments and has no
bearing on positional arity, so it buys nothing. Row `a` is the common case, and
it was refused outright. The leniency is keyed on a bare **`@_`** read alone.

## Where the answer had to live

`news/2026-08/template-mojo-triage-closed.md` had already dispositioned this and
named the fix: "a dedicated field threaded from the AST through to the runtime
`Sub` value". It also recorded what *not* to do — the one attempt made then
reserved a synthetic `params` entry, which leaked into the ~80 call sites that
read a Sub's raw `params` (multi-dispatch candidate arity matching among them).

So the flag is `CompiledCode::reads_args_array`, set by the same op-scanning
pass that already computes `writes_topic`, and the binder takes it as an
`Option<bool>` rather than deriving it from the body. Deriving it from the body
is what the first attempt here did, and it is wrong in a way worth recording: a
**compiled closure's `SubData::body` is empty** — the AST is gone once the
bytecode exists — so walking it answers a confident and wrong "no", which turned
row `b` into a spurious throw on the `&sub` Code-object path while looking
correct on the direct-call path. `Interpreter::routine_reads_args_array` asks the
`CompiledCode` when there is one, the body when the routine is interpreted, and
answers `None` (stay lenient) when it can tell neither.

The op scan also carries the trap `CLAUDE.md`'s debugging section warns about:
`GetArrayVar`'s constant is the **sigiled** name `"@_"`, where the topic's env
key is a bare `"_"`. Matching `"_"` — by analogy with the neighbouring
`writes_topic` scan — made the flag silently never fire. A `--dump-bytecode`
read settled it in one step.

While here, the "too many" message learned rakudo's singular: "expected 1
argument", not "1 arguments", reusing the `argument{}` shape the "too few" arm a
few lines up already had.

## Result

`Template::Mojo` 0.2.2 goes **4/5 → 5/5** — `00-basic` test 17 was the last
failure, and it asserts this error's text.
[#7553](https://github.com/tokuhirom/mutsu/issues/7553)'s table row can be
struck.

Pinned by `t/placeholder-sub-rejects-extra-positionals.t`, 17 assertions passing
unchanged under `raku` as well as under mutsu. Every *throwing* case in it goes
through a `&sub` Code object deliberately: rakudo rejects a literal over- or
under-supplied call to a statically-visible signature at **compile** time, so the
runtime binder check is only observable through an indirect call. mutsu has no
such static check — a separate gap, noted in the test rather than pinned.
