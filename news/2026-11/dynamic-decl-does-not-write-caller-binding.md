# A fresh `my $*X` no longer writes through a caller binding of `$*X`

```raku
sub inner() { my $h := $*HI; { my $*HI = 7 }; $h }
sub outer() { my $*HI = 3; inner() }
say outer();   # 3 (mutsu answered 7)
```

`my $h := $*HI` recorded a bind pair against the routine's local `*HI` slot
(the later `my $*HI` of the inner block), and the declaration treated that
pair as "a sibling lexical bound to the same variable", so the new value
landed in `$h`; with an immutable caller binding it died with "Cannot assign
to an immutable value" (this stopped rakudo's own `RakuAST::Deparse` on every
interpolated string). A dynamic declaration now drops the bind pairs recorded
against its slot.
