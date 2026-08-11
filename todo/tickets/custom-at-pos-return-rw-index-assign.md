# `$obj[$i] = v` on a user class with `return-rw`/`is rw` `AT-POS` (no `ASSIGN-POS`) doesn't write through

## Discovered while

Re-measuring `CSV::Table`'s own test suite after fixing the
`method dispatch:<.?>` parse bug
(`todo/tickets/method-dispatch-colon-question-syntax.md`, resolved — see
`news/2026-08/method-dispatch-colon-question-syntax.md`). With that fix,
`use CSV::Table` loads cleanly and 8/10 of its own test files run, but every
file that actually constructs a `CSV::Table` object fails during `TWEAK`
(`Text::Utils`'s `count-substrs` → `AlgorithmsIT`'s `KMP-Matcher` /
`Compute-Prefix-Function`, both operating on `AlgorithmsIT::Classes`'
`ArrayOneBased`, a user class `does Positional does Iterable`).

## Repro

```raku
class Plain does Positional {
    has @.arr;
    method AT-POS($i) is rw { @!arr[$i] }   # or: `return-rw @!arr[$i]` in the body, no `is rw` needed either way
}
my $p = Plain.new;
$p[0] = 5;
say $p.raku;   # raku: Plain.new(arr => [5])
```

```
$ mutsu tmp/positional_repro2.raku
ArrOneBased.new(arr => [-1])
ArrOneBased.new(arr => [-1])      # unchanged -- should be [-1, 0] after `$pi[1] = 0`
(Any)                              # $pi[1] read back as Any, not 0
-1
(Int)
```

(`tmp/positional_repro2.raku` in this repro, reduced from
`AlgorithmsIT::Classes.rakumod`'s `ArrayOneBased`.)

Full source of the reduced class (`ArrayOneBased` in
`~/.zef/store/AlgorithmsIT-0.0.4/*/lib/AlgorithmsIT/Classes.rakumod`):

```raku
class ArrayOneBased does Positional does Iterable {
    has @.arr;
    submethod TWEAK { @!arr[0] = -1; ... }
    method AT-POS($i) {
        return-rw @!arr[$i];
    }
    ...
}
```

`AlgorithmsIT.rakumod`'s `Compute-Prefix-Function` does `$pi[1] = 0;` where
`$pi` is a fresh `ArrayOneBased.new` (only `@!arr[0] = -1` populated by
`TWEAK`) — this assignment must extend the array via `AT-POS`'s `return-rw`
container. It silently does nothing in mutsu, so a later `$pi[$k]` read comes
back `Any` where an `Int` was expected, raising a real type-check error
downstream (`KMP-Matcher`'s `my Int $q = 0; ... $q = $pi[$q];`) and dying.

## Root cause (identified, not yet fixed)

`src/vm/vm_var_assign_index_named.rs` (~line 345-374): the postcircumfix
`$obj[i] = v` assign path, when `$obj` is a `ValueView::Instance`, only
special-cases the write when the class **explicitly declares** `ASSIGN-POS`
(`self.has_user_method(&cls, "ASSIGN-POS")`). `ArrayOneBased` never declares
`ASSIGN-POS` — real Raku's default `Positional`-role behavior is: no
`ASSIGN-POS` → fall back to calling `AT-POS(i)` and, if the returned value is
a **writable container** (the method used `return-rw` or is declared
`is rw`), assign through that container. mutsu has no such fallback branch;
when `has_user_method` finds no `ASSIGN-POS`, `method` is `None` and (per the
surrounding code, not excerpted above) the write falls through to some other
path that does not reach the class's `AT-POS` at all, so the assignment is
silently dropped.

Confirmed mutsu DOES already support this rw-container pattern for a plain
method-call lvalue (`$obj.some-method() = v` where `some-method` is `is rw`)
— see `src/runtime/methods_mut_method_lvalue.rs`. The missing piece is
specifically wiring the **postcircumfix `[]=`** path
(`vm_var_assign_index_named.rs`) to fall back to that same "call the method,
write through the returned rw container" mechanism using `AT-POS` when
`ASSIGN-POS` is absent, mirroring how `methods_mut_method_lvalue.rs`
line ~138 already converts an *explicit* `.AT-POS(idx) = v` call into
`ASSIGN-POS` — except here the fallback direction is reversed: no
`ASSIGN-POS` should fall back to `AT-POS`-as-rw-container, not the other way
around.

This is likely NOT the same thing as ADR-0001's Track B (`ContainerRef`
universal deref, fused with the GC campaign) — Track B is about built-in
Array/Hash *element* cells, whereas this is Raku's ordinary
method-call-returns-a-writable-container protocol (`is rw` / `return-rw`),
already proven working for the non-subscript case. Worth confirming this
understanding before starting, though, since both ultimately touch "does
assigning through this expression write to the right place."

## Verification

- `mutsu tmp/positional_repro2.raku` (or the `Plain` class above) should
  match raku's output exactly: the element write should be visible both via
  a subsequent `$obj[i]` read and via `.raku`/`.gist` on the object.
- Add a `t/` pin for `$obj[$i] = v` on a user `Positional` class whose
  `AT-POS` uses `return-rw` (and, separately, `is rw`), with no
  `ASSIGN-POS` declared — covering both the write-then-read-back case and
  the case where the container previously held nothing (array extension, as
  `@!arr[$i] = -1` growing from empty does in the reduction above).
- Re-run `CSV::Table`'s own suite
  (`~/.zef/store/CSV-Table-0.0.2/*/`, with `-I` pointed at its own `lib` plus
  `~/.zef/store/{Font-AFM-1.24.10,Text-Utils-4.0.2,AlgorithmsIT-0.0.4}/*/lib`)
  after the fix — it may surface further blockers past this one, or may come
  up clean.
