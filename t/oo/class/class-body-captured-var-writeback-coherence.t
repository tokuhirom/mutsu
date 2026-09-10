use Test;

# Slice F (env<->locals coherence): a deferred class-body statement that mutates
# an outer lexical writes it into `env` by name (qualified by the class package
# while the body runs); the caller frame's local slot must be written through so
# the value is coherent even with the reverse env->locals pull disabled
# (MUTSU_NO_REVERSE_SYNC=1). This pins the class-body analogue of the role-body
# write-through (the `RegisterClass` opcode drains the recorded sources).

plan 10;

# Bare scalar assignment in a class body.
{
    my $tracker = 0;
    class C1 { $tracker = 99; }
    is $tracker, 99, 'class body bare scalar assignment writes through';
}

# Expression value (a Method object) assigned to an outer lexical.
{
    my $m;
    class C2 { $m = method foo {}; }
    isa-ok $m, Method, 'class body method-expression value writes through';
    is $m.name, 'foo', 'method-expression name visible to caller';
}

# Reading another outer lexical inside the class body and writing through.
{
    my $base = 3;
    my $side = 0;
    class C3 { $side = $base * 100; }
    is $side, 300, 'class body reads outer scalar and writes outer scalar through';
    is $base, 3, 'outer scalar read is intact after class body';
}

# Compound assignment (+=) accumulating into an outer lexical.
{
    my $sum = 5;
    class C4 { $sum += 37; }
    is $sum, 42, 'class body += writes accumulated value through';
}

# Multiple distinct outer lexicals mutated in one class body.
{
    my $a = 0;
    my $b = 0;
    class C5 {
        $a = 1;
        $b = 2;
    }
    is $a, 1, 'first outer lexical writes through';
    is $b, 2, 'second outer lexical writes through';
}

# String assignment.
{
    my $name = '';
    class C6 { $name = 'composed'; }
    is $name, 'composed', 'class body string assignment writes through';
}

# The class itself remains usable after the body wrote outer state.
{
    my $hit = 0;
    class C7 {
        $hit = 1;
        method who { 'C7' }
    }
    is C7.new.who ~ ":$hit", 'C7:1', 'class is usable and body side effect visible';
}
