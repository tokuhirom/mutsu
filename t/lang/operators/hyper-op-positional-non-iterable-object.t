use Test;
use NativeCall;

# LLM::RetrievalAugmentedGeneration t/01 test 10 expects `sum($^a <<*>> $^b)` to FAIL
# for two CArray arguments: a CArray is Positional but not Iterable, so rakudo's three
# `Hyper` candidates tie (X::Multi::Ambiguous). mutsu answered 0.

plan 12;

class P does Positional {
    method list { (1, 2) }
    method AT-POS($i) { $i + 1 }
    method elems { 2 }
}

my $a = CArray[num64].new(1e0, 2e0);
my $b = CArray[num64].new(3e0, 4e0);
my @l = 1e0, 2e0;
my $p = P.new;

throws-like { $a <<*>> $b }, X::Multi::Ambiguous, 'two CArrays';
throws-like { @l <<*>> $b }, X::Multi::Ambiguous, 'a list and a CArray';
throws-like { $b <<*>> @l }, X::Multi::Ambiguous, 'a CArray and a list';
throws-like { $p <<*>> $p }, X::Multi::Ambiguous, 'two user Positional objects';

is-deeply ($a >>*>> 2), $(2e0, 4e0), 'a CArray and a scalar expand as its list';
is-deeply (2 <<*<< $a), $(2e0, 4e0), 'a scalar and a CArray';
is-deeply ($p >>*>> 2), $(2, 4), 'a user Positional object and a scalar';

is-deeply ($a.list <<*>> $b.list), $(3e0, 8e0), 'the explicit lists work';
is-deeply (@l <<*>> @l), [1e0, 4e0], 'plain arrays are unchanged';

# The prefix form has no second operand to tie on: it expands the object's list.
is-deeply (-« $a), (-1e0, -2e0), 'a prefix hyper over a CArray';
is-deeply (-« $p), (-1, -2), 'and over a user Positional object';
throws-like { $a <<*>> $b }, X::Multi::Ambiguous, message => /CArray/,
    'the ambiguity message names the operand type';
