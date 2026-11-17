use nqp;
use Test;

# #17627: `is IterationBuffer` inherits the methods, not the VMArray
# representation, so a subclass without `is repr('VMArray')` is P6opaque and
# the positional nqp ops refuse its instances, as rakudo does. A subclass that
# declares the representation, and IterationBuffer itself, keep working.

plan 9;

my class SubBuf is IterationBuffer { }

throws-like { nqp::push(nqp::create(SubBuf), 42) }, Exception,
    message => 'This type (SubBuf) does not support positional operations',
    'nqp::push refuses a P6opaque subclass';
throws-like { nqp::atpos(nqp::create(SubBuf), 0) }, Exception,
    message => 'This type (SubBuf) does not support positional operations',
    'nqp::atpos refuses it';
throws-like { nqp::elems(nqp::create(SubBuf)) }, Exception,
    message => 'This type (SubBuf) does not support elems',
    'nqp::elems refuses it';

# The representation is not inherited either: a subclass of a VMArray class
# is P6opaque again.
my class VBuf is IterationBuffer is repr('VMArray') { method m { 1 } }
my class WBuf is VBuf { }
throws-like { nqp::push(nqp::create(WBuf), 1) }, Exception,
    message => 'This type (WBuf) does not support positional operations',
    'a subclass of a VMArray class is P6opaque';

my $v := nqp::create(VBuf);
nqp::push($v, 1);
nqp::push($v, 2);
is nqp::elems($v), 2, 'a VMArray subclass takes nqp::push';
is nqp::atpos($v, 1), 2, 'and nqp::atpos';

my $b := nqp::create(IterationBuffer);
nqp::push($b, 'a');
is nqp::elems($b), 1, 'IterationBuffer itself takes nqp::push';
is nqp::atpos($b, 0), 'a', 'and nqp::atpos';
is nqp::shift($b), 'a', 'and nqp::shift';
