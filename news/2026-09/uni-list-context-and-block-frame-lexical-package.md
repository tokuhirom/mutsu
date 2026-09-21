# A Uni now flattens to its codepoints in list context, and indirect calls restore their own lexical package

Working `ecosystem/` distribution `Terminal::WCWidth` (locked on
[#7884](https://github.com/tokuhirom/mutsu/issues/7884)) surfaced two
independent, general-purpose interpreter bugs.

**A `Uni` (and its `NFC`/`NFD`/`NFKC`/`NFKD` forms) was not iterable in list
context.** `Uni` does `Positional[uint32]`, so a `for`-loop or `.map`/`.grep`
over one must iterate its codepoints — `$str.NFC.map(&wcwidth)` is exactly
how `Terminal::WCWidth`'s own `wcswidth` walks a string. `value_to_list`
(both the shared `utils::list` copy and the near-duplicate
`Interpreter::value_to_list` in `ops_compare.rs`) had no arm for
`ValueView::Uni`, so it fell through to the "single scalar item" default,
handing the whole `Uni` value to the block instead of its codepoints — which
broke binding it to a typed `Int:D` parameter with "expected Int:D but got
NFC". Fixing that naively also broke a bracket-array literal's one-arg
flatten rule (`["a".NFC]` incorrectly became `[97]`): a `Uni` is Positional
but **not** Iterable, so — like `Buf`/`Blob` and `Set`/`Bag`/`Mix` already
special-cased there — `exec_make_array_op` needs its own exemption to keep a
`Uni` whole in a literal, matching raku (`["a".NFC].raku` stays
`[Uni.new(0x0061).NFC]`). Caught by the existing
`t/nativecall/nested-leaf-repr-residues.t` pin during `make test`, and now
also covered by `t/collections/lazy-seq/uni-list-context-iteration.t`.

**An indirect call (`&sub` reference, `.map`/`.grep` callback, bare block
call) never restored its own lexical package.** `push_block_routine_with_location`
— the block/closure-call counterpart of `push_routine_with_location` — hardcoded
`lexical_package: None` instead of deriving it from the callee's `def_file`
via `lexical_package_for_frame`, the way the named-call path already does.
Without it, a bareword resolved inside such a call (an imported symbol, a
type) was looked up against the *caller's* package rather than the callee's
own defining package. Pinned by
`t/modules/import-export/imported-constant-visible-via-code-ref.t`.

A third, deeper finding stayed open: a sibling module's imported `constant`
is still invisible to a `my sub`'s body when that sub is invoked indirectly
from *within another named sub's own call frame* (as opposed to directly, or
indirectly from the mainline script body) — filed as
[#8905](https://github.com/tokuhirom/mutsu/issues/8905), since it needs the
compiler's constant-folding/free-variable machinery, not a call-frame fix.
That is what keeps `Terminal::WCWidth`'s own `ecosystem/` record `red`: its
`wcwidth` is reached through `Test::Util`'s `assert-length` helper, itself a
named sub doing `$str.NFC.map(&wcwidth)`.
