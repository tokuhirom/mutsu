use Test;

# Found via the CSS::Font::Resources suite: `my CSS::Module::Property $meta =
# $carray[$i]` where the CArray[CStruct] AT-POS returns a Proxy. `=` reads its
# RHS in value context, so the declaration checks the FETCHed value.

plan 4;

class K { has $.n }

class A {
    has @!s = K.new(n => 4);
    method AT-POS($p) is rw {
        Proxy.new(FETCH => -> $ { @!s[$p] }, STORE => -> $, $v { @!s[$p] = $v });
    }
}

my $a = A.new;
my K $x = $a[0];
is $x.n, 4, 'typed my with an AT-POS Proxy initializer';

my K $y = Proxy.new(FETCH => -> $ { K.new(n => 2) }, STORE => -> $, $v { });
is $y.n, 2, 'typed my with a fresh Proxy initializer';

sub g() is rw { Proxy.new(FETCH => -> $ { K.new(n => 3) }, STORE => -> $, $v { }) }
my K $w = g();
is $w.n, 3, 'typed my initialized from an is rw routine returning a Proxy';

throws-like { my Int $i = Proxy.new(FETCH => -> $ { "str" }, STORE => -> $, $v { }) },
    X::TypeCheck::Assignment, 'a Proxy whose FETCH has the wrong type still fails the check';

done-testing;
