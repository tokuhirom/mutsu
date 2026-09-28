use Test;

# A bare `Whatever` held in a variable (not curried into a WhateverCode,
# which is built at parse time) and a bare `Block`/`Sub` have no `.Numeric`
# candidate at all: forcing either into a numeric context is
# X::Multi::NoMatch, not a silent 0 (found by the 2026-09-27 doc-diff sweep,
# #9791).
plan 10;

{
    my $x = *;
    my $name;
    { ($x + 2).sink; CATCH { default { $name = .^name } } }
    is $name, 'X::Multi::NoMatch', 'Whatever + Int is X::Multi::NoMatch';
}

{
    my $x = *;
    my $name;
    { (-$x).sink; CATCH { default { $name = .^name } } }
    is $name, 'X::Multi::NoMatch', 'prefix - on Whatever is X::Multi::NoMatch';
}

{
    my $b = { 1 };
    my $name;
    { ($b + 1).sink; CATCH { default { $name = .^name } } }
    is $name, 'X::Multi::NoMatch', 'Block + Int is X::Multi::NoMatch';
}

{
    my $b = { 1 };
    my $name;
    { ([+] $b).sink; CATCH { default { $name = .^name } } }
    is $name, 'X::Multi::NoMatch', '[+] on a single Block is X::Multi::NoMatch';
}

{
    my $b = { 1 };
    my $name;
    { (+$b).sink; CATCH { default { $name = .^name } } }
    is $name, 'X::Multi::NoMatch', 'prefix + on a Block is X::Multi::NoMatch';
}

{
    my $b = { 1 };
    my $c = { 2 };
    my $name;
    { ([+] $b, $c).sink; CATCH { default { $name = .^name } } }
    is $name, 'X::Multi::NoMatch', '[+] over two Blocks is X::Multi::NoMatch';
}

{
    my $b = { 1 };
    my $msg;
    { ($b + 1).sink; CATCH { default { $msg = .message } } }
    is $msg,
        "Cannot resolve caller Numeric(Block:D: ); none of these signatures matches:\n    (Mu:U \\v: *\%_)",
        'the message names Block, matching Rakudo';
}

# Untouched: a single-element reduce with no identity still passes an
# uncoercible element through unchanged, and ordinary numeric reduction
# is unaffected.
{
    my $b = { 1 };
    my $result = [max] $b;
    ok $result === $b, '[max] on a single Block still passes it through';
}
my $sum = [+] 1, 2, 3;
is $sum, 6, 'plain [+] reduction still works';
my $one-elem = [+] "2";
is $one-elem, 2, '[+] on a single numeric-looking Str still numifies';
