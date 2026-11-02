use Test;
use lib 't/fixtures/attr-build-thunk-unit/lib';
use BuildThunkCaller;
use BuildThunkDep;

# Found via Red t/082-emakei: `has Str $.id = ~UUID.new` evaluated through
# `Attribute.build` from another module died with "Undeclared name: UUID",
# because the thunk ran in the caller's compilation unit.
plan 3;

class C { has Str $.id = ~BuildThunkDep.new; }

is C.new.id, 'dep-id', 'the default works in the declaring unit';
my $attr = C.^attributes.head;
is $attr.build.(Mu, Mu), 'dep-id', '.build called in the declaring unit';
is run-attr-build($attr), 'dep-id', '.build called from another module sees the declaring unit\'s imports';
