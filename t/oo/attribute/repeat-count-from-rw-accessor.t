use Test;

# Found via RedFactory: `(.title ~ "\n") x .PAR("x-times")` where PAR is an
# `is rw` method returning a hash element. The count arrived as the accessor's
# container and was read as 0, so the string came out empty.

plan 8;

class F {
    has %!P;
    method p($n) is rw { %!P{$n} }
    method q($n) is rw { my $x = 3; $x }
}
my $f = F.new;
$f.p("k") = 4;

is "ab" x $f.p("k"), 'abababab', 'x with an is-rw hash-element accessor as the count';
is "ab" x $f.q("k"), 'ababab', 'x with an is-rw scalar accessor as the count';
is ("ab" x $f.p("k")).chars, 8, 'and the length is right';
is (("ab") xx $f.p("k")).elems, 4, 'xx with the same count';
is 1 x $f.p("k"), '1111', 'integer left operand';

my %h = a => 2;
sub r() is rw { %h<a> }
is "z" x r(), 'zz', 'x with an is-rw sub call as the count';
is "z" x (r()), 'zz', 'parenthesised';
my $blk = { ("t" ~ "\n") x .p("k") };
is $blk($f), "t\nt\nt\nt\n", 'inside a block with the invocant as the topic';
