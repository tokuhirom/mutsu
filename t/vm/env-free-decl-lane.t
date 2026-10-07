use Test;

plan 10;

# Declarations whose slot is the variable's only home skip the name-keyed
# bookkeeping (#12151); each shape below must still behave as before.

# A typed native declaration in a loop body re-seeds every iteration.
{
    my int $total = 0;
    for 1..3 -> int $i {
        my int $seen;
        $total += $seen;
        $seen = $i;
    }
    is $total, 0, 'a typed declaration without initializer starts at 0 each iteration';
}

# The declared type still guards later stores.
{
    my int $x = 5;
    dies-ok { $x = "abc" }, 'a native int refuses a Str';
    is $x, 5, 'and keeps its value';
    my str $s = "a";
    dies-ok { $s = 5 }, 'a native str refuses an Int';
}

# A declaration's initializer of the wrong type is refused.
dies-ok { my int $y = "oops"; }, 'wrong-typed initializer dies';

# A cell left in env by an earlier `is rw` binding does not leak into a fresh declaration.
{
    sub bump($v is rw) { $v++ }
    my $p = 10;
    bump($p);
    for 1..2 {
        my $p = 100;
        $p++;
        is $p, 101, 'a fresh $p in a loop body is independent of the rw-bound outer one';
    }
    is $p, 11, 'the outer variable keeps its bumped value';
}

# Shadowing: an inner declaration leaves the outer one untouched.
{
    my int $v = 1;
    for 1..2 { my int $v = 7; $v++ }
    is $v, 1, 'a loop-local typed shadow does not clobber the outer variable';
}

# repl() reads the caller's lexicals by name, so it forces the by-name store.
{
    my $out = run($*EXECUTABLE, '-e',
        'my $name = "Alice"; repl(); say "Goodbye, $name"',
        :in, :out, :err);
    $out.in.say('$name = "Bob"');
    $out.in.close;
    like $out.out.slurp(:close), /'Goodbye, Bob'/, 'repl() sees and assigns the lexical';
}
