use Test;

# A method-call target inside a list assignment writes through the container
# the method returns, like the item assignment `$o.x = v` (#11230).

plan 9;

class G {
    has $.x is rw;
    has $.v;
    method !p is rw { $!v }
    method set(\a, \b) { (self!p, my $z) = a, b; $z }
}

{
    my $f = G.new;
    ($f.x,) = 5,;
    is $f.x, 5, 'one-element list assignment to an rw accessor';
}

{
    my $f = G.new;
    ($f.x, my $y) = 5, 6;
    is $f.x, 5, 'rw accessor next to an inline declaration';
    is $y, 6, 'the inline declaration gets the next value';
}

{
    my $f = G.new(x => 1);
    my ($p, $q) = 10, 20;
    ($p, $f.x, $q) = $f.x, $p, 9;
    is-deeply ($p, $f.x, $q), (1, 10, 9), 'RHS is read before any target is written';
}

{
    my $f = G.new;
    my $n = 'x';
    ($f."$n"(), *) = 7, 8;
    is $f.x, 7, 'indirect method-call target';
}

{
    my @a = 1, 2, 3;
    my $p;
    (@a.AT-POS(1), $p) = 20, 30;
    is-deeply (@a, $p), ([1, 20, 3], 30), '.AT-POS target';
}

{
    my $f = G.new;
    my $p;
    is-deeply (($f.x, $p) = 11, 12), (11, 12), 'the assignment yields the targets';
}

{
    my $f = G.new;
    ($f.x, my @rest) = 1, 2, 3;
    is-deeply ($f.x, @rest), (1, [2, 3]), 'slurpy array after an accessor target';
}

{
    my $g = G.new;
    is-deeply ($g.set(3, 4), $g.v), (4, 3), 'private rw method target';
}
