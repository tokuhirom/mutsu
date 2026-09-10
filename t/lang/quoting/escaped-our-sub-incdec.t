use v6;
use Test;

# An `our sub` declared in a bare block closes over the block's `my` lexicals.
# The registry routine outlives the block, so a call made AFTER the block exits
# must still see (and mutate) the captured lexical through its persisted shared
# cell. This exercises the inc/dec path (`$a++`), which previously bypassed the
# escaped-our-sub cell and read Nil (0), losing the initialization and updates.
# Regression for roast S02-names-vars/variables-and-packages.t tests 32-34.

plan 8;

# postfix increment, called after the block exits (nested wrapper block)
{
    {
        my $a = 3;
        our sub grtz { $a++ }
    }
    is &OUR::grtz(), 3, 'escaped our sub: postfix ++ sees init (1)';
    is &OUR::grtz(), 4, 'escaped our sub: postfix ++ accumulates (2)';
    is &OUR::grtz(), 5, 'escaped our sub: postfix ++ accumulates (3)';
}

# a plain `my sub` that merely shares the variable name must NOT route through
# the escaped cell — it is called while its own block is still live.
{
    my $a;
    sub rmbl { $a++ }
    is rmbl(), 0, 'plain my sub captures its own var (1)';
    $a++;
    is rmbl(), 2, 'plain my sub captures its own var (2)';
}

# prefix increment through an escaped our sub
{
    {
        my $b = 10;
        our sub bump { ++$b }
    }
    is &OUR::bump(), 11, 'escaped our sub: prefix ++ sees init and mutates (1)';
    is &OUR::bump(), 12, 'escaped our sub: prefix ++ accumulates (2)';
}

# decrement through an escaped our sub
{
    {
        my $c = 5;
        our sub drop { $c-- }
    }
    is &OUR::drop(), 5, 'escaped our sub: postfix -- sees init (1)';
}
