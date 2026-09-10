use v6;
use Test;

# §1.3 S14: $OUTER::name must resolve the binding visible exactly `depth`
# lexical scopes out. Under MUTSU_SHADOW_SLOTS each shadow owns a distinct
# local slot and the runtime's by-name position search picks the OUTERMOST
# slot, wrong for depth 1 of a triple shadow — the compiler now bakes the
# emit-point slot onto GetOuterVar. Pin for
# roast/S02-names-vars/variables-and-packages.t tests 10/15.
# Must pass with MUTSU_SHADOW_SLOTS unset (default) AND =1, and on real raku.

plan 5;

{ my $a = 1; {
   my $a=2; {
      my $a=3;
      is $a, 3,               'innermost shadow';
      is $OUTER::a, 2,        '$OUTER::a is the middle binding';
      is $OUTER::OUTER::a, 1, '$OUTER::OUTER::a is the outermost binding';
}}}

# same shape, different name + post-block restore check. (Depth-2 access in a
# SECOND block group, and depth access from a block that does not itself
# declare the name, both go through pre-existing default-build snapshot quirks
# that are wrong even without shadow slots — out of scope for this pin.)
{ my $b = 10; {
    my $b = 20; {
        my $b = 30;
        is $OUTER::b, 20, '$OUTER::b sees the enclosing shadow';
    }
    is $b, 20, 'enclosing shadow unchanged after the inner block';
}}
