use Test;
use nqp;

# ADR-14162: a class's BUILDPLAN is readable through the ClassHOW mixin
# hooks, in Rakudo's encoding (task code, type, '$!name', then the named
# argument or the reason).

plan 8;

my @seen;
my role PlanDump {
    method create_BUILDPLAN(Mu \type) {
        callsame;
        my \plan := nqp::getattr(self, Metamodel::ClassHOW, '@!BUILDPLAN');
        my \all := nqp::getattr(self, Metamodel::ClassHOW, '@!BUILDALLPLAN');
        my @tasks;
        for ^nqp::elems(plan) -> $i {
            my $t := nqp::atpos(plan, $i);
            @tasks.push: nqp::islist($t)
                ?? nqp::hllize(nqp::atpos($t, 0)) ~ ':' ~ nqp::atpos($t, 2)
                !! 'code:' ~ $t.name;
        }
        my @all;
        for ^nqp::elems(all) -> $i {
            my $t := nqp::atpos(all, $i);
            @all.push: nqp::islist($t)
                ?? nqp::hllize(nqp::atpos($t, 0)) ~ ':' ~ nqp::atpos($t, 2)
                !! 'code:' ~ $t.name;
        }
        @seen.push: type.^name => (@tasks.join(' '), @all.join(' '));
    }
}

class A1 {
    has $.a = 1; has $.b is required; has $!c; has Int $.d;
    submethod TWEAK { }
    BEGIN { A1.HOW does PlanDump }
}
class A2 {
    has $.p;
    submethod BUILD(:$!p) { }
    BEGIN { A2.HOW does PlanDump }
}
class A3 {
    has $.r is rw; has $.z is required("why"); has Str $.w is default("d");
    BEGIN { A3.HOW does PlanDump }
}
class A4 {
    has int $.n; has num $.f; has str $.s;
    BEGIN { A4.HOW does PlanDump }
}
my %seen = @seen;

is %seen<A1>[0],
    '0:$!a 0:$!b 0:$!d 800:$!b 400:$!a 1000:$!c 1000:$!d code:TWEAK',
    'init, required, default, leave tasks, then TWEAK';
is %seen<A1>[1], '0:$!a 0:$!b 0:$!d 800:$!b 400:$!a code:TWEAK',
    'the whole-MRO plan drops the leave tasks';
is %seen<A2>[0], 'code:BUILD 1000:$!p', 'an own BUILD stands in for the init tasks';
is %seen<A2>[1], 'code:BUILD', 'BUILDALLPLAN of a BUILD class';
is %seen<A3>[0], '0:$!r 0:$!z 0:$!w 800:$!z 1000:$!r 1000:$!w',
    'is default is an element default, not a build default';
is %seen<A4>[0], '1:$!n 2:$!f 3:$!s', 'native attributes have their own codes';

my \plan := nqp::getattr(A1.HOW, Metamodel::ClassHOW, '@!BUILDPLAN');
ok nqp::islist(plan), 'the plan is an nqp list';
is nqp::elems(plan), 8, 'read back after composition';

done-testing;
