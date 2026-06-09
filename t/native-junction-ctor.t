use Test;

# VM-native dispatch for the any/all/one/none junction constructors
# (ledger §2: pure builtins lowered out of the tree-walk call_function
# terminal). Behavior must stay identical to the interpreter path.

plan 24;

# --- kind / type ---
is any(1, 2, 3).WHAT.gist, Junction.gist, 'any returns a Junction';
is all(1, 2, 3).WHAT.gist, Junction.gist, 'all returns a Junction';
is one(1, 2, 3).WHAT.gist, Junction.gist, 'one returns a Junction';
is none(1, 2, 3).WHAT.gist, Junction.gist, 'none returns a Junction';

# --- gist (element order preserved) ---
is any(1, 2, 3).gist, 'any(1, 2, 3)', 'any gist';
is all(1, 2, 3).gist, 'all(1, 2, 3)', 'all gist';
is one(1, 2, 3).gist, 'one(1, 2, 3)', 'one gist';
is none(1, 2, 3).gist, 'none(1, 2, 3)', 'none gist';

# --- one-arg flattening rule ---
is any((1, 2, 3)).gist, 'any(1, 2, 3)', 'any one-arg list flattens';
is all([4, 5, 6]).gist, 'all(4, 5, 6)', 'all one-arg array flattens';
is any(1..3).gist, 'any(1, 2, 3)', 'any one-arg range flattens';
{
    my @a = 7, 8;
    is one(@a).gist, 'one(7, 8)', 'one over @-array flattens';
}

# --- multiple args: no recursive flatten ---
is any((1, 2), 3).gist, 'any((1 2), 3)', 'multi-arg does not flatten list element';

# --- single scalar ---
is all(42).gist, 'all(42)', 'all of one scalar';

# --- matching semantics (the point of junctions) ---
ok 2 == any(1, 2, 3), 'any matches when one element matches';
nok 9 == any(1, 2, 3), 'any fails when none match';
ok 5 == all(5, 5, 5), 'all matches when every element matches';
nok 5 == all(5, 5, 6), 'all fails when one differs';
ok 1 == one(1, 2, 3), 'one matches with exactly one';
nok 1 == one(1, 1, 3), 'one fails with two matches';
ok 9 == none(1, 2, 3), 'none matches when nothing matches';
nok 2 == none(1, 2, 3), 'none fails when something matches';

# --- empty junction ---
is any().gist, 'any()', 'empty any';

# --- junction containing object (Instance) arguments still constructs ---
{
    my class P { has $.n }
    my $j = any(P.new(n => 1), P.new(n => 2));
    is $j.WHAT.gist, Junction.gist, 'any over Instance args is a Junction';
}
