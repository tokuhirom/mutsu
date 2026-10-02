use v6;
use Test;

# A hyper method call that `take`s once per element (`@a».take`) runs its
# element loop inside one opcode, which a lazy gather pull cannot resume
# mid-iteration. A take-limit hit inside it used to unwind the op and drop
# every remaining element (`(gather { @a».take }).map({ $_ })` gave `(1)`,
# #9785). The op now finishes and the gather suspends right after it.

plan 9;

{
    my @a = 1..3;
    is-deeply (gather { @a».take }).map({ $_ }).List, (1, 2, 3),
        'lazily read gather of @a».take yields every element';
    is-deeply (gather { (1, 2, 3)».take }).map({ $_ }).List, (1, 2, 3),
        'list-literal invocant';
}

{
    my @log;
    my $s = gather { (1, 2, 3)».take; @log.push('after') };
    is $s[0], 1, 'first pulled element';
    is-deeply @log, [], 'suspends right after the hyper op, before the next statement';
    is-deeply $s.List, (1, 2, 3), 'full force keeps all values once';
    is-deeply @log, ['after'], 'statement after the hyper op runs exactly once';
}

is-deeply (gather { for 1..2 { (1, 2, 3)».take; take 'x' } }).map({ $_ }).List,
    (1, 2, 3, 'x', 1, 2, 3, 'x'),
    'hyper take inside a for loop resumes the same iteration after the op';

is-deeply (gather { loop { (1, 2, 3)».take } }).head(5).List, (1, 2, 3, 1, 2),
    'hyper take inside an infinite loop stays lazy';

is-deeply (gather { for ^Inf { (1, 2, 3)».take } }).head(5).List, (1, 2, 3, 1, 2),
    'hyper take inside an infinite for loop stays lazy';
