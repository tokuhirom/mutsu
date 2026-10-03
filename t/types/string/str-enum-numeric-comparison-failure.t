use Test;

# A Str-valued enum numifies through its value, so a numeric comparison or
# arithmetic on it is the same lazy X::Str::Numeric Failure as on the string
# itself -- and the negated comparisons answer True rather than throwing
# (#11578).

plan 7;

enum T (A => "text", B => "block");

isa-ok (B == A), Failure, '== on a Str enum is a Failure';
is B != A, True, '!= on a Str enum is True';
is B !== A, True, '!== on a Str enum is True';
is A != A, True, '!= is True even for the same Str enum value';
isa-ok (B < A), Failure, '< on a Str enum is a Failure';
isa-ok (B + 1), Failure, 'arithmetic on a Str enum is a Failure';

enum N (P => "1", Q => "2");
is P < Q, True, 'a numeric-string enum still compares by its value';
