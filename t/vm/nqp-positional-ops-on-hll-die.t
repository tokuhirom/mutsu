use v6;
use Test;
use nqp;

# The positional nqp ops are REPR operations: a high-level List/Array (and a
# Str, Seq, Range, Capture) is P6opaque and dies, a VMArray (raw list,
# `$!reified`, native array, Buf, Uni, CArray) works. A Hash/Map delegates
# the associative ops and `elems` to its storage. Messages checked against
# Rakudo (ADR-12666 §3.9, #12671).
plan 27;

my @a = 1, 2, 3;
my %h = a => 1;
my $list := (1, 2);

sub dies($code, $msg, $desc) { throws-like $code, Exception, message => $msg, $desc }

dies { nqp::elems(@a) }, 'This type (Array) does not support elems', 'elems on an Array';
dies { nqp::elems($list) }, 'This type (List) does not support elems', 'elems on a List';
dies { nqp::push(@a, 4) }, 'This type (Array) does not support positional operations', 'push';
dies { nqp::pop(@a) }, 'This type (Array) does not support positional operations', 'pop';
dies { nqp::shift(@a) }, 'This type (Array) does not support positional operations', 'shift';
dies { nqp::unshift(@a, 0) }, 'This type (Array) does not support positional operations', 'unshift';
dies { nqp::splice(@a, nqp::list(), 0, 1) }, 'This type (Array) does not support positional operations', 'splice';
dies { nqp::atpos($list, 0) }, 'This type (List) does not support positional operations', 'atpos on a List';
dies { nqp::existspos(@a, 0) }, 'This type (Array) does not support elems', 'existspos';
dies { nqp::iterator(@a) }, 'Cannot iterate object with P6opaque representation (Array)', 'iterator on an Array';
dies { nqp::iterator(%h) }, 'Cannot iterate object with P6opaque representation (Hash)', 'iterator on a Hash';
dies { nqp::elems('abc') }, 'This type (Str) does not support elems', 'elems on a Str';
dies { nqp::elems((1, 2).Seq) }, 'This type (Seq) does not support elems', 'elems on a Seq';
dies { nqp::elems(1..3) }, 'This type (Range) does not support elems', 'elems on a Range';
dies { nqp::elems(Array) }, 'elems requires a concrete object (got a Array type object instead)',
    'elems on a type object';
is @a.elems, 3, 'the refused ops left the Array alone';

is nqp::elems(%h), 1, 'elems on a Hash delegates to its storage';
is nqp::atkey(%h, 'a'), 1, 'atkey on a Hash';
is nqp::elems(nqp::getattr(@a, List, '$!reified')), 3, 'elems on $!reified';
my str @s = 'a', 'b';
is nqp::elems(@s), 2, 'elems on a native str array';
is nqp::elems(Buf.new(1, 2)), 2, 'elems on a Buf';
is nqp::elems("aé".NFD), 3, 'elems on a Uni';

my $l = nqp::list(1, 2);
is nqp::islist($l), 1, 'a raw list assigned to a $ variable stays raw';
is nqp::elems($l), 2, '... and the positional ops work on it';
is (-> $x { nqp::islist($x) })($l), 0, 'binding a block parameter still hllizes';
is nqp::islist(nqp::radix(10, '12', 0, 0)), 1, 'nqp::radix answers a raw list';
is nqp::islist(nqp::split(',', 'a,b')), 1, 'nqp::split answers a raw list';
