use Test;

# Binding (`:=`) to a sigilless `constant` at statement level must raise,
# not silently no-op. Previously `name := value` for a bareword LHS parsed
# into a meaningless block `{ name; value }` that evaluated both sides and
# discarded the bind, so the error never fired.

plan 6;

{
    constant baka = 42;
    my $ok = 0;
    $ok++ if baka == 42;
    try { EVAL 'baka := 23' };
    $ok++ if $!;            # the bind must throw
    $ok++ if baka == 42;    # ... and leave the constant unchanged
    is $ok, 3, 'binding an Int to a constant throws and does not change it';
}

{
    constant wibble = 42;
    my $ok = 0;
    $ok++ if wibble == 42;
    try { EVAL 'wibble := { 23 }' };
    $ok++ if $!;
    $ok++ if wibble == 42;
    is $ok, 3, 'binding a block to a constant throws and does not change it';
}

# Assignment to a constant already raised; keep it covered here too.
{
    constant wobble = 7;
    dies-ok { EVAL 'wobble = 99' }, 'assigning to a constant dies';
    is wobble, 7, 'constant value is preserved after a failed assignment';
}

# A sigilless `\term` is readonly as well; reading it still works.
{
    my \term = 5;
    is term, 5, 'sigilless term reads back its value';
    dies-ok { EVAL 'term := 6' }, 'rebinding a sigilless term dies';
}
