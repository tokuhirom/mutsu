use Test;

# AttrX::Mooish: a closure nested in a block calls the block's `is raw` /
# `is rw` sub through its lexical binding (the same as for a plain sub), keeping
# the container the sub returns.
plan 5;

my @r;
for 1, 2 -> $v {
    my $q = $v;
    my sub raw-h(\x) is raw { $q + x }
    my sub rw-h(\x) is rw { $q }
    @r.push(-> { raw-h(10) });
    @r.push(-> { rw-h(0) });
}
is @r.map({ $_() }).join(","), "11,1,12,2", 'each closure calls its own activation\'s sub';

my $store = 5;
{
    my sub cell() is rw { $store }
    my $c = -> { cell() = 9 };
    $c();
}
is $store, 9, 'assignment through an is rw sub called from a closure';

{
    my sub rawlist(*@a) is raw { @a }
    my $f = -> { rawlist(1, 2, 3) };
    is $f().elems, 3, 'an is raw slurpy sub called from a closure';
}

{
    my @arr = 1, 2, 3;
    my sub first-el() is raw { @arr[0] }
    my $g = -> { first-el() = 99 };
    $g();
    is @arr.join(","), "99,2,3", 'is raw sub hands back the element container';
}

{
    my sub hidden(Mu \o, $a) is raw is hidden-from-backtrace { $a }
    my $h = anon method () { hidden(self, 7) };
    is 5.$h, 7, 'is raw + hidden-from-backtrace called from an anon method';
}
