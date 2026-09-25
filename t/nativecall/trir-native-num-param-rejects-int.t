use Test;

# An `Int` does not unbox to a native `num`: raku dies "This type cannot
# unbox to a native number". TRIR's read-only parameter binder converted it
# to a float instead, so `sub f(num $x) { $x }` (a body TRIR compiles)
# answered 5 while the general binder, used for any body TRIR declines,
# died. The result depended on the body's shape.

plan 4;

sub ident(num $x) { $x }
sub named(num $x) { $x.^name }

my $five = 5;
dies-ok { ident($five) }, 'TRIR-compiled body rejects an Int for num';
dies-ok { named($five) }, 'general binder rejects an Int for num';
is ident(5e0), 5e0, 'a Num still binds';
my $r = 0;
$r += ident($_.Num) for 1..3;
is $r, 6e0, 'Num arguments in a loop still bind';
