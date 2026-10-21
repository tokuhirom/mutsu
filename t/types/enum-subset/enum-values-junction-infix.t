use Test;

# ADR-14162 (AttrX::Mooish's `when pvmForce | pvmNever`): two enum values
# joined by `|` are the any-junction, not a call of the first with the slip
# of the second.

plan 5;

enum Mode <force never auto>;
my $junction = force | never;
ok $junction ~~ Junction, 'infix | over two enum values';
ok never ~~ $junction, 'matches a member';
nok auto ~~ $junction, 'and rejects a non-member';

sub pick(Mode $m) {
    given $m {
        when force | never { 'fn' }
        when auto { 'auto' }
    }
}
is pick(never), 'fn', 'in a when clause';
is pick(auto), 'auto', 'the other branch';

done-testing;
