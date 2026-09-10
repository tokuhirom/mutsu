use Test;

# Pin: a `let`/`temp` restore (on block exit or exception) writes the saved
# value back into the env store; the matching local slot must be reconciled so a
# later read sees the restored value WITHOUT relying on reverse-sync
# (`MUTSU_NO_REVERSE_SYNC=1`). Previously the restore was env-only and the locals
# slot kept the in-block value, so with reverse-sync off `temp`/`let` appeared
# not to restore. This must hold identically whether or not reverse-sync runs
# (raku is the oracle).

plan 10;

# temp: always restores at scope exit
{
    my $x = 1;
    { temp $x = 42; }
    is $x, 1, 'temp: restored after block (locals coherent)';
}

# temp: bare form, mutate inside, restore on exit
{
    my $x = 10;
    { temp $x; $x = 99; }
    is $x, 10, 'bare temp: restored after block (locals coherent)';
}

# let: kept on success
{
    my $x = 1;
    { let $x = 42; }
    is $x, 42, 'let: kept on successful exit (locals coherent)';
}

# let: restored when the block yields a falsy value (Nil)
{
    my $x = 1;
    { let $x = 42; Nil }
    is $x, 1, 'let: restored on Nil result (locals coherent)';
}

# let: restored on exception (try)
{
    my $x = 1;
    try { let $x = 42; die "oops" }
    is $x, 1, 'let: restored after exception (locals coherent)';
}

# nested temp: inner and outer restored independently
{
    my $x = 1;
    { temp $x = 2; { temp $x = 3; }; is $x, 2, 'nested temp: inner restored'; }
    is $x, 1, 'nested temp: outer restored (locals coherent)';
}

# temp restore is visible to subsequent arithmetic on the restored local
{
    my $n = 5;
    { temp $n = 100; }
    my $after = $n + 1;
    is $after, 6, 'temp-restored local feeds later expression';
}

# let with bare form kept on success, readable afterwards
{
    my $s = "a";
    { let $s; $s = "b"; 1 }
    is $s, "b", 'bare let: kept on success (locals coherent)';
}

# temp on a hash element restores the whole container coherently
{
    my %h = a => 1;
    { temp %h<a> = 9; }
    is %h<a>, 1, 'temp hash element restored (locals coherent)';
}
