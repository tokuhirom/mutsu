# `Template::Mustache` "Context Precedence" test regressed (pre-existing, not from DBIish bundling)

Found while running `scripts/battery-testsuite.sh --update` for the DBIish
bundling work: `modules/Template-Mustache`'s own upstream `t/01-basic.rakutest`
subtest 2 ("Context Precedence: Dotted names should be resolved against former
resolutions") now fails, along with `06-logging`, `11-iterable`,
`12-inheritence`, `50-readme`, `91-specs`, `92-specs-file` — all previously in
`batteries-whitelist.txt`.

**Confirmed NOT caused by the DBIish session's changes**: reproduces identically
against a clean `git stash` of every file this session touched
(`src/vm/vm_call_exec_ops.rs`, `src/runtime/methods_mut_rw_attr.rs`,
`src/runtime/methods_mut_method_lvalue.rs`) — i.e. plain `main` at
`5c169c530`. The whitelist entries were **restored by hand** in the DBIish
bundling PR rather than silently dropped, so this regression stays visible to
the release gate instead of being erased from history.

## Repro

```raku
use Template::Mustache;
say Template::Mustache.render(
    '{{#a}}{{b.c}}{{/a}}',
    { a => {b => {}}, b => {c => 'ERROR'} }
);
# raku:  ''
# mutsu: 'ERROR'
```

i.e. mutsu resolves the dotted name `b.c` inside the `{{#a}}` section against
the *outer* context's `b` (`{c => 'ERROR'}`) instead of the properly-scoped
inner context established by entering the section (`a`'s own value, `{b =>
{}}`, whose `b` is empty).

## Where to look

`modules/Template-Mustache/lib/Template/Mustache.rakumod`, the `format(%val,
@context)` multi's nested `get`/`visit`/`resolve` subs (~line 397-465). The
context stack `@context` is walked innermost-first via
`@context.map({visit($^ctx, @field[0])})`, and the first *defined* result
should short-circuit (`last`) — for this input, `visit` on the innermost frame
(`a`'s value, `{b=>{}}`) should already yield the empty Hash for `b` and stop,
never falling through to the outer frame's `b => {c=>'ERROR'}`. If that
short-circuit is not happening (or `.defined` classifies the empty Hash
differently than expected), the outer frame's `b` gets picked up instead.

`Template::Mustache` itself has built-in trace logging
(`self.log: :level<Trace>, ...` throughout `get`/`visit`) — passing
whatever enables that (check `Template::Mustache.new(:$log-level)` or similar
in the module) against both `raku` and `mutsu` side by side on the minimal
repro above is the fastest way to see where the two diverge, rather than
guessing.

## Why not fixed in the DBIish PR

Unrelated to DBIish/NativeCall — a general hash/array/context-stack semantics
question in unrelated code, discovered as a side effect of running the full
battery gate. Investigating and fixing it belongs in its own session so it gets
a real root-cause rather than a guess bolted onto an unrelated PR. Candidate
suspects (not verified): recent general fixes to hash-key/Array-with-nilish-
element handling landed in `main` around the same time
(`fix: DBIish upstream Pg+mysql suite parity — nine general fixes (#5555)`,
in particular its hash-attribute type-object-key coercion and the `[Any]`
scalar-store change) — worth checking first since they are the most recent
general-purpose changes to adjacent code, but this has not been bisected.
