use Test;

plan 6;

# A Seq subclass whose is-lazy says True: elems / end soft-fail instead of
# pulling the endless iterator.
my class LazySeq is Seq { method is-lazy() { True } }
my $s = LazySeq.new((^Inf).iterator);

my $e = $s.elems;
isa-ok $e, Failure, ".elems on a lazy Seq subclass is a Failure";
isa-ok $e.exception, X::Cannot::Lazy, "... holding X::Cannot::Lazy";
is $e.exception.action, ".elems", "... with the .elems action";
$e.so;

my $n = $s.end;
isa-ok $n, Failure, ".end is a Failure too";
$n.so;

fails-like { LazySeq.new((^Inf).iterator).elems }, X::Cannot::Lazy, action => '.elems';

# A non-lazy subclass still counts.
my class FiniteSeq is Seq { }
is FiniteSeq.new((1, 2, 3).iterator).elems, 3, "a finite Seq subclass still counts";
