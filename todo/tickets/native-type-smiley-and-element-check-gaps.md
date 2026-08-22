# Four native-type / smiley parity gaps found while closing the `NativeHelpers::Blob` ticket

Found while measuring Gap B and Gap C of the (now retired)
`todo/deep/nativehelpers-blob-moarvm-guts.md` — see
[`news/2026-08/nativehelpers-blob-parity-gaps-closed.md`](../../news/2026-08/nativehelpers-blob-parity-gaps-closed.md),
whose "Still not in scope" section lists them. They were deliberately left out
of that PR because none has a bundled-battery consumer and none was what the
ticket was about. Filing them here so they are tracked as *open findings*
rather than as a paragraph inside a completed-work entry.

All four re-verified against `main` on 2026-08-22.

## 1. A boxed `Int` smartmatches as a native type

```raku
say 5 ~~ int;     # raku: False   mutsu: True
say 5 ~~ uint8;   # raku: False   mutsu: True
```

A boxed `Int` is not a native `int`. mutsu answers `True` for every native
integer type. Likely the same over-broad numeric-type check for the whole
`int`/`uint`/`intN`/`uintN` family; check whether `num`/`num32`/`num64` and
`str` behave the same way before fixing, and whether the `.isa` path (which
[#6843](https://github.com/tokuhirom/mutsu/pull/6843) corrected for `array`)
already gets this right.

## 2. Assigning an element of a native array checks against the CONTAINER's type

```raku
my array[uint8] $a .= new(1, 2);
$a[0] = 7;
# raku : [7 2]
# mutsu: Type check failed for an element of $a; expected array[uint8] but got Int (7)
```

The element-store type check compares the assigned value against the
container's declared type (`array[uint8]`) instead of its element type
(`uint8`). Note this reproduces through the `array[T] $x` spelling; the shaped
`my uint8 @a` spelling assigns fine, so the two element-store paths disagree
and the fix should make them agree rather than patching one.

Confirmed pre-existing (verified against a build without #6843's changes), so
it is not a regression from that PR.

## 3. `.^name` keeps the `:_` smiley

```raku
say (Int:_).^name;   # raku: Int   mutsu: Int:_
```

raku normalizes `:_` away in the type object's name (`:_` is the "either"
smiley, i.e. no constraint at all). `:D`/`:U` are kept by both. Worth checking
`.gist`/`.raku`/`.WHAT` for the same leak while fixing.

## 4. A smiley type object has no `.ACCEPTS`

```raku
say (Int:D).ACCEPTS(5);      # raku: True   mutsu: No such method 'ACCEPTS' for invocant of type 'Int:D'
say (Str:U).ACCEPTS(Str);    # raku: True   mutsu: No such method 'ACCEPTS' for invocant of type 'Str:U'
```

Smartmatch against the same smiley works (`5 ~~ Int:D` is `True`), so the
constraint logic exists — it is only unreachable through the explicit
`.ACCEPTS` method call, i.e. the smiley type object is not being resolved to
something that carries the method. #6843's news entry notes a related raku
quirk to *not* copy: rakudo answers `True` for the literal `$a ~~ array:U` but
`False` for `(array:U).ACCEPTS($a)`; mutsu's self-consistent `False` there is
deliberate, so fixing this item must not "fix" that case into agreement with
the literal form.

## Priority

Low — no bundled battery needs any of them, which is exactly why they were
split out. Items 3 and 4 look small and self-contained; item 2 is a real
correctness bug that any user of `array[T]` would hit; item 1 is a smartmatch
soundness gap that could plausibly mask a type error in user code.
