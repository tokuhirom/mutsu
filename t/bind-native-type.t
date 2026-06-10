use Test;

# A natively-typed variable (`my int $x`, `my num $n`, `my str $s`) is not a
# container, so it cannot be bound with `:=` — Raku raises X::Bind::NativeType.
# Boxed types (Int/Str) and ordinary assignment are unaffected.

plan 7;

throws-like 'my int $x := 2', X::Bind::NativeType, 'int bind', name => '$x';
throws-like 'my num $n := 2e0', X::Bind::NativeType, 'num bind', name => '$n';
throws-like 'my str $s := "x"', X::Bind::NativeType, 'str bind', name => '$s';
throws-like 'my int8 $b := 1', X::Bind::NativeType, 'int8 bind';

# Native assignment still works.
is (my int $x = 5), 5, 'native int assignment';

# Boxed types can still be bound.
lives-ok { my Int $y := 3; die unless $y == 3; }, 'boxed Int bind works';

# Untyped bind still works.
lives-ok { my $z := 7; die unless $z == 7; }, 'untyped bind works';
