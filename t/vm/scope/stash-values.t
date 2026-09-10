use v6;
use Test;

# `Foo::.values` returns the stash's symbol values. The native `.values` fast
# path used to claim the Stash instance and return (the stash itself,).

enum Day <Mon Tue Wed>;

is-deeply Day::.values.sort, (Mon, Tue, Wed).sort.List,
    'enum stash .values yields the enum members';
is-deeply Day::.keys.sort.List, ("Mon", "Tue", "Wed"),
    'enum stash .keys yields the member names';

# Bool is a built-in enum; its stash exposes False/True.
is Bool::.values.elems, 2, 'Bool stash has two values';
ok Bool::.values.grep(* === True), 'Bool stash values include True';
ok Bool::.values.grep(* === False), 'Bool stash values include False';
is-deeply Bool::.keys.sort.List, ("False", "True"), 'Bool stash keys';

# The X cross of stash values iterates all combinations (JSON::Fast t/12).
my @combos = Bool::.values X Bool::.values;
is @combos.elems, 4, 'X over Bool stash values gives 4 combos';

done-testing;
