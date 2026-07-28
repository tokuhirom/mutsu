# A punned *parameterised* role never runs its `BUILD`

`R[T].new` on a parameterised role builds an object whose `submethod BUILD` never
ran. The same shape on a plain `class` works, and so does a punned role with no
type parameter (that was #5495).

```raku
role R2[::T] {
    has @!cache handles <AT-POS>;
    submethod BUILD() {
        @!cache := Array[T].new(:shape(2));
        @!cache[0] = T.new;
        note "inside BUILD";        # raku prints this; mutsu never does
        self
    }
    method new(::?CLASS:U:) { self.bless }
}
say R2[Int].new[0].^name;           # raku: Int   mutsu: Nil
```

Swap `role R2[::T]` for `class C1` (with the type written out) and mutsu prints
`inside BUILD` and answers correctly, so the delegation, the private array
attribute and the `:=` bind are all fine. The parameterised-role pun is the
trigger.

## Why it matters

It is what stops `NativeHelpers::CStruct`'s `LinearArray` — the last piece of the
`NativeHelpers` surface `DBIish`'s mysql driver uses:

```raku
role LinearArray[::T] does Positional[T] {
    has Pointer $!storage;
    has @!cache handles <AT-POS elems shape>;
    submethod BUILD(:$!size!, :$!storage!, :$!managed) { ... }
    method new(::?CLASS:U: Int $size) {
        with calloc($size, $sol) -> $storage { self.bless(:$size, :$storage, :managed) }
    }
}
```

`LinearArray[MYSQL_BIND].new($n)` returns an object with an empty cache, so
`$arr[$i]` is `Nil` and every field assignment on it dies with
`X::Assignment::RO: cannot assign through .buffer on non-instance`.

## A second, separate bug in the same file

A `my` in a **role body** initialised from the role's type parameter reads as 0:

```raku
role R[::T] {
    my int $sol = nativesizeof(T);
    method sizeof() { $sol }
}
say R[int64].new.sizeof;    # raku: 8   mutsu: 0
```

`LinearArray` computes its element stride that way (`my int $sol =
nativesizeof(T)`), so even with `BUILD` fixed the stride would be 0 and every
element would alias element 0. Either `T` is not yet bound when the role body's
`my` runs, or the role-body `my` the method closes over is not the one the body
initialised.

Reduction for both:
[`tmp/linear-reduce.raku`](../../tmp) shape is reproduced above; neither needs a
database, `NativeLibs`, or a native library beyond `calloc`.

## Context

Found while landing [ADR-0015](../../docs/adr/0015-native-backed-container-storage-and-repr-bodies.md)'s
P1 (`BODY_OF` over a native handle), whose stated acceptance was "`LinearArray`
allocates, indexes and disposes". The REPR-body mechanism itself is done and
verified against the real `MoarVM::Guts::REPRs`; these two are what remain
between it and `LinearArray`, and neither is NativeCall work.
