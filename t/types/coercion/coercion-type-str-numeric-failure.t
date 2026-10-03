use Test;

# A coercion type converts a Str the way the `Str.Int`/`.Num`/`.Rat`/
# `.Complex` method does: a non-numeric string becomes the lazy
# X::Str::Numeric Failure, never a silent 0 (#11291).

plan 14;

sub f(Int() $o) { $o }
isa-ok f("x"), Failure, 'Int() of a non-numeric Str is a Failure';
throws-like { f("x") + 1 }, X::Str::Numeric, 'using it throws X::Str::Numeric';
is f("42"), 42, 'Int() of a decimal Str';
is f(" 7 "), 7, 'surrounding whitespace is allowed';
is f("0x10"), 16, 'radix prefix parses like Str.Int';
is f("3/2"), 1, 'rational form truncates like Str.Int';
is f("12345678901234567890123"), 12345678901234567890123, 'big integer kept exactly';

my Int() $v = "y";
throws-like { $v + 0 }, X::Str::Numeric, 'coercing variable declaration fails the same way';

my @seen;
for "z" -> Int() $o { @seen.push: $o }
isa-ok @seen[0], Failure, 'for-loop coercion parameter fails the same way';

sub g(Num() $n) { $n }
isa-ok g("x"), Failure, 'Num() of a non-numeric Str is a Failure';

sub h(Rat() $n) { $n }
is h("1.5").raku, '1.5', 'Rat() of a decimal Str';
isa-ok h("x"), Failure, 'Rat() of a non-numeric Str is a Failure';

sub c(Complex() $n) { $n }
isa-ok c("x"), Failure, 'Complex() of a non-numeric Str is a Failure';

sub e(Int(Str) $o) { $o }
throws-like { e("z") + 0 }, X::Str::Numeric,
    message => /'must begin with valid digits'/, 'Int(Str) reports the parse error';
