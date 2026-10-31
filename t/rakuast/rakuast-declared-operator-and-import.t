use experimental :rakuast;
use Test;
use lib $?FILE.IO.parent(2).add('lib');
use CheckTimeUnitProbe;

# static::Analyzer runs from a module's scope but reads the importing unit's
# tree (#15610): a routine the unit imported resolves to a Routine, and an
# operator the unit declares resolves to that routine, not the setting's.
# Expected values are rakudo 2026.09's.

plan 4;

sub infix:<~~~>(Int $a, Int $b) { 1 }
my @calls;
my @operators;
CHECK {
    @calls = probe-resolution('probe-sample');
    sub walk($node) {
        return unless $node ~~ RakuAST::Node;
        @operators.push: $node if $node ~~ RakuAST::Infix && $node.operator eq '~~~';
        $node.visit-children({ walk($_) });
    }
    walk($*CU);
}

my $x = probe-sample();
my $y = 1 ~~~ 2;

is @calls.elems, 1, 'the imported routine is called once';
ok @calls[0], 'it resolves to a Routine from the module\'s scope';
is @operators.elems, 1, 'the declared operator is in the tree';
ok @operators[0].resolution.compile-time-value ~~ Routine, 'it resolves to the unit\'s own routine';
