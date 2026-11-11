use Test;

# A `proto method`'s object (#16888, EEL 0.3 mixes a role into its `proto
# pattern`): a role mixed into the dispatcher `.^find_method` answers stays on
# it for later lookups, and a proto with no multi candidates is still a
# dispatcher, over none.

plan 6;

role Tagged { has $.tag is rw }

class WithMulti {
    proto method m(|) {*}
    multi method m(Int $x) { 'int' }
}
my $p = WithMulti.^find_method('m');
$p does Tagged;
$p.tag = 'family';
is WithMulti.^find_method('m').tag, 'family', 'the role stays on the proto';
nok WithMulti.^find_method('m').candidates[0].can('tag'), 'its candidate is left alone';
is WithMulti.m(3), 'int', 'the proto still dispatches';

class Lone {
    proto method pick(|) {*}
    method pick:sym<a> { 'a' }
}
my $lone = Lone.^find_method('pick');
ok $lone.is_dispatcher, 'a candidate-less proto is a dispatcher';
is $lone.candidates.elems, 0, 'over no candidates';
$lone does Tagged;
ok Lone.^find_method('pick').can('tag'), 'a role mixed into it stays';
