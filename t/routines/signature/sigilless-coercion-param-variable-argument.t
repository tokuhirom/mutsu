use Test;

# Pop (Pop::Rect.new: 3 -> Pop::Point's `!SET-SELF(Num() \x, Num() \y)`) binds a
# sigilless coercion-typed parameter to a variable argument. The parameter is
# raw, so mutsu aliased the caller's variable and dropped the coerced value:
# `Num() \y` given `my $k = 3` still held the Int and a native `num` attribute
# then refused it.

plan 11;

sub num-name(Num() \y)  { y.^name }
sub num-name-s(Num() $y) { $y.^name }
sub int-name(Int() \y)  { y.^name }
sub str-name(Str() \y)  { y.^name }

my $k = 3;
my $s = 4.5;
my @a = 3;

is num-name($k),      'Num', 'a scalar variable is coerced by a sigilless Num() parameter';
is num-name-s($k),    'Num', 'as it is by a $ parameter';
is int-name($s),      'Int', 'Int() of a Rat variable';
is str-name($k),      'Str', 'Str() of an Int variable';
is num-name(@a[0]),   'Num', 'an array element argument is coerced';
is num-name($k + 0),  'Num', 'an expression argument is coerced';
is num-name(3),       'Num', 'a literal argument is coerced';
is $k, 3, 'the caller variable is unchanged';
is $k.^name, 'Int', 'and keeps its type';

class P {
    has num $.x;
    has num $.y;
    method new(Real \x, Real \y = x) { self.bless!SET-SELF(x, y) }
    method !SET-SELF(Num() \x, Num() \y) { $!x = x; $!y = y; self }
}
my $w = 3;
is P.new($w).y, 3, 'a defaulted sigilless argument reaches a native num attribute';
is P.new($w).y.^name, 'Num', 'as a Num';
