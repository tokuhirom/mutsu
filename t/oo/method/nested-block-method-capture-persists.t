use Test;

# From Test::Stream (Test::Stream::Hub.instance): methods declared in a bare
# block of a class body close over the block's lexicals, and a write one call
# makes is seen by the next call and by the block's sibling methods.

plan 9;

class Reg {
    {
        my Reg $instance;
        method instance ($class: |c) { return $instance //= $class.new(|c) }
        method clear { $instance = (Reg) }
    }
    {
        my $other = 0;
        method bump { ++$other }
    }
}

my $a = Reg.instance;
ok $a === Reg.instance, 'a `//=` singleton persists across calls';
Reg.clear;
ok $a !=== Reg.instance, 'a sibling method clearing it is seen by the next call';
is Reg.bump + Reg.bump, 3, 'a second block keeps its own variable';

class Cnt { { my $n = 0; method inc { $n++; $n } method get { $n } } }
Cnt.inc; Cnt.inc;
is Cnt.get, 2, 'postfix ++ in one method is read by a sibling';

class P3 { { my $i = 0; method m { $i = $i + 1; $i } } }
is P3.m, 1, 'plain assignment, first call';
is P3.m, 2, 'plain assignment, second call';

class Un { { my $i; method m { $i //= 5; $i++ } } }
is Un.m, 5, 'uninitialised lexical, first call';
is Un.m, 6, 'uninitialised lexical, second call';

# A typed lexical keeps its own constraint when the caller has a same-named
# lexical of another type.
class Hub2 {
    {
        my Hub2 $instance;
        method instance ($class: |c) { return $instance //= $class.new(|c) }
    }
}
my Str $instance;
my $h = Hub2.instance;
ok $h === Hub2.instance, "the caller's `my Str \$instance` does not constrain the method's";
