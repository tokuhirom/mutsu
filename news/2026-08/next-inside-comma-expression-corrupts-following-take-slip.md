# A `FIRST` phaser that exits via `next` no longer re-fires on every iteration

From the doc-diff harness (`Type/Mu.rakudoc:515`):

```raku
sub insert($sep, +@list) {
    gather for @list {
        FIRST .take, next;
        take slip $sep, .item
    }
}
say insert ':', <a b c>;   # raku: (a : b : c)   mutsu: (a b c)
```

## Root cause — not the array-literal machinery the ticket suspected

The ticket hypothesized that `next`, thrown out of the middle of building the
`(.take, next)` comma expression, left the VM's argument-collection state dirty
and corrupted the later `take slip(...)`. It does not. `--dump-ast` and a plain
`for` reduction told a much simpler story:

```
$ mutsu -e 'for 1..3 { FIRST next; say "x", $_ }'
(no output at all; raku prints x2, x3)
```

`Compiler::expand_loop_phasers` lowers a loop `FIRST` phaser to

```
if $__mutsu_loop_first_ { <FIRST body>; $__mutsu_loop_first_ = False }
```

with the "already ran" assignment **after** the body. A `next` (or `last`, or
`return`) thrown out of the FIRST body skips that trailing assignment, so the
flag stayed `True` and FIRST fired again on *every* subsequent iteration. With a
bare `FIRST next` that skipped the entire loop; with `FIRST .take, next` it
re-ran the `.take` each time, which is exactly the observed `(a b c)` — three
FIRST takes and no separators, not a corrupted `slip`.

## Fix

The flag is now cleared **before** the phaser body runs, so FIRST fires exactly
once no matter how its body leaves. Every measured case then matches `raku`:
the documented `insert` idiom, the `FIRST { .take; next }` block spelling, the
bare `FIRST next` (`(: b : c)`), and the plain-loop `for 1..3 { FIRST next; say
"x", $_ }`.

Pinned by `t/lazy-gather-and-junction.t`.
