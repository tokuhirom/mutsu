use Test;

# `@@x` / `@(@x)` / `@$o` on a user class that does Positional take the
# object's `.list` (#15286, from annotations t/01-containers.rakutest).

plan 4;

class Buf1 does Positional {
    has @!b = 1..3;
    method elems { 3 }
    method AT-POS(Int:D $p) is raw { @!b[$p] }
    method list(::?CLASS:D:) { @!b }
}

my @buffer := Buf1.new;
is-deeply @@buffer, [1, 2, 3], '@@ on a := bound user Positional';
is-deeply @(@buffer), [1, 2, 3], '@( ) on the same binding';
my $o = Buf1.new;
is-deeply @$o, [1, 2, 3], '@$ on a scalar holding one';
is @@buffer.WHAT.^name, 'Array', '@@ answers an Array';
