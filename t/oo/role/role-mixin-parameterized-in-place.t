use v6;
use Test;

# `$o does R[...]` reblesses the object into the type `C+{R[...]}` composed
# with those arguments, as a plain `$o does R` does: the role's TWEAK sees
# its parameters, keeps what it binds into the object's attributes, and a
# closure it leaves behind writes the same attributes the methods read.
# Same as raku.

plan 17;

role R[$a?, Bool:D :$reset = True, :$x] {
    has $.ln is rw = 0;
    has &!os;
    has $.seen;
    submethod TWEAK { $!seen = "reset=$reset"; sub zero { $!ln = 0 }; &!os := &zero }
    method a { $a }
    method x { $x }
    method bump { ++$!ln }
    method fire { &!os() }
}
class C { }

my $c = C.new;
$c does R[1, :x];
is $c.^name, 'C+{R[Int]}', 'a positional argument shows as its type';
ok $c ~~ R, 'does the role';
ok $c ~~ R[Int], 'does the parameterization';
nok $c ~~ R[Str], 'not another parameterization';
is $c.a, 1, 'a method sees the positional argument';
is $c.x, True, 'a method sees the named argument';
is $c.seen, 'reset=True', 'TWEAK sees a defaulted parameter';
$c.bump; $c.bump;
is $c.ln, 2, 'methods write the role attribute';
$c.fire;
is $c.ln, 0, 'a closure TWEAK bound writes the same attribute';
$c.bump;
is $c.ln, 1, 'and methods see that write';

my $d = C.new;
$d does R[2];
is $d.a, 2, 'another object mixed with other arguments';
is $c.a, 1, 'leaves the first one alone';
nok $c.WHAT === $d.WHAT, 'they are different types';

my $e = C.new;
$e does R[:!reset];
is $e.^name, 'C+{R}', 'named arguments only: no brackets';
is $e.seen, 'reset=False', 'TWEAK sees the named argument';

my $f = C.new;
$f does R;
is $f.seen, 'reset=True', 'a plain does binds the parameter defaults for TWEAK';

my $h = IO::CatHandle.new;
$h does R[:x];
ok $h ~~ IO::CatHandle && $h.x, 'a built-in instance is reblessed too';
