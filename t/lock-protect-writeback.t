use Test;

# A captured outer lexical assigned inside `Lock.protect: { ... }` must be
# visible to the caller after the protected block returns. The block runs
# inline in the current env, so the free-var write lands in env and must be
# reconciled back into the caller's local slot.

plan 4;

{
    my $lock = Lock.new;
    my $x = 1;
    $lock.protect: { $x = 42; };
    is $x, 42, 'plain assignment inside protect writes back';
}

{
    my $lock = Lock.new;
    my $x = 1;
    $lock.protect: { $x = $x + 100; };
    is $x, 101, 'read-modify-write inside protect writes back';
}

sub in-sub() {
    my $lock = Lock.new;
    my $x = 5;
    $lock.protect: { $x = 7; };
    $x;
}
is in-sub(), 7, 'protect writeback works inside a sub';

{
    # No semicolon after the protect block, followed by `if` on the next line:
    # the `if` must be a separate statement, not a postfix modifier.
    my $lock = Lock.new;
    my $x = 1;
    $lock.protect: {
        $x = 9;
    }
    if (! $x) { $x = -1 }
    is $x, 9, 'block-final protect statement self-terminates before if';
}
