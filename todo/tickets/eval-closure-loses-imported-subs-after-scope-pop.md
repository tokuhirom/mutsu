# Closure returned from EVAL loses subs imported by a `use` inside the EVAL

`Template::HAML`'s `t/0040-haml-render.rakutest` aborts with
`Unknown function: haml-indent` ("planned 2, ran 0", exit 255). This is a
pre-existing deterministic bug: it reproduces identically on v0.20.0
(1b1fa6112), on #5565, and on #5566, with and without precomp
(`MUTSU_PRECOMP=0`), so it is unrelated to the export-scan cache work. raku
passes the file. (The 0.33s timing recorded for this file in
`news/2026-07/module-export-scan-cache.md` was wall-clock of the aborting run —
the exit status was not checked at the time.)

## Minimal repro (verified 2026-07-30, raku prints 14)

```raku
# OpMod.rakumod:  unit module OpMod; sub exported-fn(Int $n --> Int) is export { $n * 2 }
use MONKEY-SEE-NO-EVAL;
my $f = EVAL 'use OpMod; sub (Int $n) { exported-fn($n) }';
say $f(7);   # mutsu: "Unknown function: exported-fn" / raku: 14
```

The narrower shapes all work: `EVAL 'use OpMod; exported-fn(7)'` (call inside
the EVAL) is fine, and a closure over a sub imported by the *outer* scope is
fine. The failure needs all three: the `use` is inside the EVAL, the EVAL
returns a closure, and the closure is called after the EVAL scope is gone.

## Root-cause direction

Same family as the import-scope pop bugs: when the EVAL's import scope is
popped, the bare imported name (`exported-fn`) is dropped from the function
registry — correctly, for the *importer's* scope — but the closure created
inside that scope should still resolve it lexically. #5566 fixed the class
variant (qualified class names retained across the pop) and functions already
retain `::`-qualified keys, but a bare imported sub name referenced only from
an escaping closure has nothing to keep it alive. The likely fix direction is
the escape-gate approach used for block-scoped `my sub` leaking (capture or
pin the resolution for closures that escape the scope), not blanket retention.

## How this bites Template::HAML

`DirectCodegen.rakumod` line 90 does `EVAL $body` where the generated `$body`
starts with `use Template::HAML::DirectEmit;` (line 846) and defines the
render sub, which calls DirectEmit's exported `haml-indent`. The render sub is
called later, after the EVAL has returned, so the first render dies. Blocks
`t/0040-haml-render.rakutest` (and any HAML render via the direct-codegen
path); dist at `tmp/haml-perf/dist` (from
`~/.cache/mutsu-dist-sweep/T_EM_TEMPLATE_HAML_*.tar.gz`).
