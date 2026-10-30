use v6;
use Test;

plan 3;

# An undeclared class named by a `returns` / `of` trait is X::InvalidType.
for 'sub f() returns Foo { 1 }', 'sub f() of Foo { 1 }', 'sub f() is export returns Foo { 1 }' -> $code {
    throws-like { EVAL $code }, X::InvalidType, "$code dies with X::InvalidType";
}
