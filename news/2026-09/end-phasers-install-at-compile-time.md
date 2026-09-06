# END phasers now install at compile time, including the ones never reached

Rakudo installs **every** `END` phaser a compunit declares when that compunit
is *compiled* — walking the source top to bottom — and runs the installed
phasers in reverse at exit. mutsu installed one only when execution reached its
`PhaserEnd` opcode, which lost two things at once: an `END` that execution
never reaches never ran at all, and the phasers that did run were ordered by
their source *line*, which ties when several share one physical line. Both are
fixed.

## What was measured

Against rakudo 2026.07, before any code changed:

```raku
if False { END { say "never-run-block" } }
sub g      { END { say "uncalled-sub" } }
for 1..3   { END { say "loop" } }
END        { say "main" }
```

```
raku : main  loop  uncalled-sub  never-run-block
mutsu: main  loop
```

An `END` inside a never-entered block, an uncalled sub, or a method of a class
nobody instantiates still runs. The measurement also settled the question the
ticket left open — what such a phaser's body sees for the lexicals of the block
it was declared in:

```raku
if False { my $x = 1; END { say $x.^name; say $x.defined } }   # raku: Any, False
```

The declaring scope's lexicals exist and are simply *undefined*, because nothing
ever assigned them.

Two further divergences turned up in the same sweep, neither of them in the
ticket:

```raku
sub f($n) { my $v = $n * 10; END { say $v } }
f(1); f(2); f(3);                       # raku: 30      mutsu was: 10

for 1..3 -> $i { END { say $i } }        # raku: 3       mutsu was: 1
```

An `END` reached more than once still runs exactly once, but it closes over the
frame of the **last** execution that reached it, not the first.

And the residual the earlier source-line ordering fix left behind:

```raku
{ END { say 1 } }; { END { say 2 } }; END { say 3 }   # raku: 3 2 1, mutsu: 2 1 3
```

## What changed

**A per-declaration source index replaces the source line.** The parser is a
strictly left-to-right recursive descent, so numbering each `END` node as it is
built (`ast::next_end_phaser_index`, stored in the new
`Stmt::Phaser::end_index`) reproduces exactly the order rakudo's compiler
installs them in — top-level, block-scoped and sub-scoped ENDs all in one
sequence, with no tie possible between two on one line. `end_order::slot` keys
a main-compunit phaser by that index; a module's and an `EVAL`'s ENDs keep the
registration sequence, which for those classes *is* their install order.

**A pre-pass installs them all before the body runs.** The new
`runtime::end_preregister` walks the parsed main compunit in source order and
calls `preinstall_end_phaser` for every `END` it finds, remembering the slot in
`main_end_slots`. This subsumes the old eager hoist of the top-level ENDs (which
existed so an `END` still runs when the mainline dies) and extends it to every
nesting depth. A phaser installed this way starts with an *empty* captured env,
so at exit every name its body mentions resolves against the live exit-time env
rather than being overlaid with a stale copy.

**Reaching the declaration now captures rather than installs.**
`exec_phaser_end_op` first tries `capture_end_phaser_env(end_index)`: on a hit
it overwrites that slot's env and package and returns, so the phaser keeps the
install position the pre-pass gave it. Because a re-reached declaration
re-captures, the last execution wins, which is what made the `sub f`/`for` cases
above line up with rakudo. The old `site_id` de-duplication is still there, but
only on the fallback path, which is now just a module's ENDs, an `EVAL`'s, an
rvalue `END { }`, and any nesting form the walker does not descend into — that
last case degrades gracefully, because the *ordering* key is the parser's index
either way.

**The scope-death freeze needed a new marker.** `update_end_phaser_envs` used
`end_phasers.len()` as a "phasers registered inside this scope" mark, which
stops meaning anything once every phaser is installed up front: the vector no
longer grows when a scope registers one. `EndPhaser::capture_seq` plus
`Interpreter::end_phaser_capture_mark()` replace it — a scope records the
capture counter on entry and freezes exactly the captures at or after it.

## The trap that cost the most time

`t/end-phaser-module-order.t` failed in a way that reproduced only in one
specific temp directory and nowhere else, with identical file contents. The
cause was the **precompilation cache**: `Stmt::Phaser` is serialized into it, so
a module AST cached by one process replayed *that* process's `end_index` into a
later one, where the same number named a main-compunit slot — and the module's
END silently captured into the script's phaser instead of installing itself.
`end_index` is `#[serde(skip)]` now (a module's ENDs are ordered by load order
and never by a main-compunit index, so a cache hit loses nothing). The general
lesson: a process-global counter is only unique per process, and the precomp
cache outlives the process.

## Pins

`t/end-phaser-compile-time-install.t` (new, 9 cases: unreached block / uncalled
sub / uninstantiated method, doubly-nested dead branch, both one-line ordering
directions, last-execution capture for a sub and a loop, an unreached END's
undefined lexical and auto-vivified container, and a dead-branch END that still
runs when the mainline dies). `t/end-phaser-source-order.t` and
`t/end-phaser-module-order.t` are unchanged and still pass, as does all of
`roast/S04-phasers/`.

## What is left

One property is still off, and is recorded as
`todo/tickets/unreached-end-phaser-lexicals-read-nil-not-any.md`: an unreached
`END` that mentions a lexical of its own (never-run) declaring block reads it as
`Nil` where rakudo gives `Any`. Both are undefined and `.defined` agrees, so it
shows up only through `.^name` / `.raku`. The fix needs the pre-pass to seed the
lexical names visible at the declaration point, which is a scope analysis mutsu
has no single authority for — deliberately out of scope here.
