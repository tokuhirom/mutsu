use Test;

# An array element bound to a Proxy mediates its own store when the array is
# reached through an expression (`get()[1] = 5`, `$obj.m[1] = 5`), as through
# a variable (CSS::Properties' `$css.margin[1] = 20px`).
plan 5;

my %store = a => 1, b => 2;
sub item($k) is rw {
    Proxy.new(FETCH => -> $ { %store{$k} // 0 }, STORE => -> $, $v { %store{$k} = $v })
}
my @bound;
my $n = 0;
@bound[$n++] := item($_) for <a b>;

@bound[1] = 20;
is %store<b>, 20, 'through the variable';

sub get() is rw { @bound }
get()[1] = 5;
is %store<b>, 5, 'through a sub call';

class C { method n() is rw { @bound } }
C.new.n[1] = 7;
is %store<b>, 7, 'through a method call on a temporary';

my $c = C.new;
$c.n[1] = 9;
is %store<b>, 9, 'through a method call on a variable';

%store<b>:delete;
is @bound[1], 0, 'the element still reads through its Proxy';
