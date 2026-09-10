use Test;

# Pin for VM-native Iterator protocol dispatch (ledger §1: native receiver
# dispatch -> VM-native). `$it.pull-one` etc. compile to CallMethodMut; the
# index-advancing dispatch over a self-contained array-backed iterator runs in
# the VM (try_native_iterator), writing the advanced index back by instance
# identity exactly as the interpreter's mutating iterator dispatch does. Lazy /
# squish / predictive iterators stay on the interpreter, so behaviour matches.

plan 18;

# --- pull-one over a List iterator ---
{
    my $i = (1, 2, 3).iterator;
    is $i.pull-one, 1, 'pull-one #1';
    is $i.pull-one, 2, 'pull-one #2';
    is $i.pull-one, 3, 'pull-one #3';
    ok $i.pull-one =:= IterationEnd, 'pull-one at end is IterationEnd';
    ok $i.pull-one =:= IterationEnd, 'pull-one stays IterationEnd';
}

# --- pull-one over an Array iterator ---
{
    my @a = <a b c>;
    my $i = @a.iterator;
    is $i.pull-one, 'a', 'Array iterator pull-one';
    is $i.pull-one, 'b', 'Array iterator pull-one #2';
}

# --- pull-one over a (finite) Range iterator ---
{
    my $i = (10 .. 13).iterator;
    my @got;
    loop {
        my $x = $i.pull-one;
        last if $x =:= IterationEnd;
        @got.push($x);
    }
    is @got, [10, 11, 12, 13], 'Range iterator fully drained via pull-one';
}

# --- skip-one ---
{
    my $i = (1, 2, 3).iterator;
    ok $i.skip-one, 'skip-one returns True with elements left';
    is $i.pull-one, 2, 'skip-one advanced past the first';
    ok $i.skip-one, 'skip-one #2 (skips the last element)';
    nok $i.skip-one, 'skip-one returns False when exhausted';
}

# --- skip-at-least ---
{
    my $i = (1 .. 10).iterator;
    ok $i.skip-at-least(3), 'skip-at-least(3) succeeds';
    is $i.pull-one, 4, 'skip-at-least advanced by 3';
    nok $i.skip-at-least(100), 'skip-at-least past the end returns False';
    ok $i.pull-one =:= IterationEnd, 'iterator exhausted after over-skip';
}

# --- iterator advance persists through a local variable inside a sub ---
{
    sub drain() {
        my $i = (1, 2, 3, 4).iterator;
        $i.skip-at-least(2);
        $i.pull-one;
    }
    is drain(), 3, 'iterator state advances correctly in a sub-local slot';
}

# --- alias observes the advance (same iterator object) ---
{
    my $i = (1, 2, 3).iterator;
    my $j := $i;
    $i.pull-one;
    is $j.pull-one, 2, 'aliased iterator shares advanced state';
}
