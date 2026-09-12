use Test;

# `$i ⚛-= $n` is the subtract half of the atomic compound assignment whose add
# half (`⚛+=`) mutsu already had. Both spellings rakudo declares —
# U+002D HYPHEN-MINUS and U+2212 MINUS SIGN — were already listed in
# `runtime::core_infix_names` as operators mutsu claims to know, but no parser
# site ever recognised either, so the statement failed to parse outright
# (Async::Workers, FFmpegProgressBar, Russian).
#
# The two operators are the same read-modify-write on the same target and
# differ only in the sign of the delta, so both lower to
# `__mutsu_atomic_add_var` with the subtract forms negating their right-hand
# side. The negation is applied to the delta expression, before the atomic
# update runs, so atomicity is unchanged.

plan 13;

{
    my atomicint $i = 5;
    $i ⚛-= 2;
    is ⚛$i, 3, 'the statement form subtracts';
}

{
    my atomicint $i = 5;
    my $r = ($i ⚛-= 2);
    is $r, 3, 'the expression form yields the new value';
    is ⚛$i, 3, 'and leaves it in the variable';
}

{
    my atomicint $i = 5;
    $i ⚛−= 2;          # U+2212 MINUS SIGN
    is ⚛$i, 3, 'the U+2212 MINUS SIGN spelling subtracts too';
}

# The negation must bind the whole delta, not just its first term.
{
    my atomicint $i = 5;
    $i ⚛-= 1 + 1;
    is ⚛$i, 3, 'a compound right-hand side is negated as a whole';
}

{
    my atomicint $i = 5;
    my $n = 10;
    $i ⚛-= $n;
    is ⚛$i, -5, 'a variable right-hand side subtracts, and may go negative';
}

# The operators that already worked must keep working.
{
    my atomicint $i = 5;
    $i ⚛+= 2;
    is ⚛$i, 7, 'atomic add still works';

    my atomicint $j = 5;
    $j ⚛= 9;
    is ⚛$j, 9, 'atomic assign still works';

    my atomicint $k = 5;
    $k⚛++;
    is ⚛$k, 6, 'atomic postfix increment still works';

    my atomicint $l = 5;
    --⚛$l;
    is ⚛$l, 4, 'atomic prefix decrement still works';
}

# `⚛=` as a DECLARATION's initializer (`my $qu ⚛= $!queue-unblock;` in
# Async::Workers). On a variable the declaration is only now creating there is
# nothing to be atomic against — no other thread can hold a reference to it
# yet — so rakudo treats it as an ordinary initialization. mutsu accepted the
# operator only in an assignment to an already-declared variable, so the
# declaration form failed to parse.
{
    my atomicint $q = 3;
    my $qu ⚛= $q;
    is $qu, 3, 'an atomic store initializes a `my` declaration';

    my atomicint $i ⚛= 5;
    $i ⚛-= 2;
    is ⚛$i, 3, 'and the declared variable still takes atomic updates afterwards';
}

# Repeated subtraction accumulates, so the lowering is not losing the update.
{
    my atomicint $i = 100;
    $i ⚛-= 7 for ^5;
    is ⚛$i, 65, 'repeated atomic subtraction accumulates';
}
