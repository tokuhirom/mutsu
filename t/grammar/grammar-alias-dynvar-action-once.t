use Test;

# An alias (`<operand=atom>`) files one cursor under two names, so its action runs
# once, also in a grammar that declares a `:my $*` dynamic variable (whose
# reduce-time bookkeeping copied the shared capture node per slot) and whose rule
# is reached through a grammar method (#16888, EEL's `eel-local`).

plan 6;

my %count;
class Count { method atom(Mu $/) { %count<atom>++ } }

grammar ViaMethod {
    method TOP { self.body }
    token body { :my $*X = False; <operand=atom> }
    token atom { 'a' }
}
%count = ();
ok ViaMethod.parse('a', :actions(Count.new)), 'parses';
is %count<atom>, 1, 'the aliased rule reduces once';

grammar Nested {
    method TOP { self.body }
    token body { :my $*X = False; <piece>+ % ' ' }
    token piece { <operand=atom> [ '%' <separator=atom> ]? }
    token atom { \w }
}
%count = ();
ok Nested.parse('a b%c', :actions(Count.new)), 'parses with a quantified alias';
is %count<atom>, 3, 'each atom reduces once';

my $m = Nested.parse('a');
ok $m<piece>[0]<operand> === $m<piece>[0]<atom>[0], 'both names hold the one cursor';

grammar Plain {
    token TOP { <operand=atom> }
    token atom { 'a' }
}
%count = ();
Plain.parse('a', :actions(Count.new));
is %count<atom>, 1, 'without a dynamic variable too';
