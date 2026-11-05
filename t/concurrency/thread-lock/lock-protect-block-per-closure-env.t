use Test;
plan 8;

# `Lock.protect` runs a block literal inline. What the run reads from and writes
# back to the enclosing frame depends on the closure INSTANCE (its captured
# variables), while the scan of the block's bytecode is shared by every instance
# of one literal. These pin that the two stay apart.
my $lock = Lock.new;

sub make-adder($start) {
    my $count = $start;
    sub () { $lock.protect: { $count++ } }
}
my &a = make-adder(10);
my &b = make-adder(100);
is a(), 10, 'first closure instance starts from its own capture';
is b(), 100, 'a second instance of the same literal has its own capture';
is a(), 11, 'the first instance kept its own count';
is b(), 101, 'and so did the second';

# A block literal in a loop is a fresh closure each iteration.
my @seen;
for 1..3 -> $i {
    $lock.protect: { @seen.push($i * 10) };
}
is-deeply @seen, [10, 20, 30], 'each iteration sees its own loop variable';

# Writes to an outer lexical made inside the block reach the caller.
my $total = 0;
for 1..4 { $lock.protect: { $total += $_ } }
is $total, 10, 'an outer lexical updated inside the block is seen by the caller';

# The block's value is returned, and a nested protect on the same lock re-enters.
is $lock.protect({ 6 * 7 }), 42, 'the block value is returned';
is $lock.protect({ $lock.protect({ 5 }) + 1 }), 6, 'a nested protect re-enters the lock';
