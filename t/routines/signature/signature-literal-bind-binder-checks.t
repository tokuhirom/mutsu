use Test;

# `:(...) := capture` binds through the signature binder, so every check a
# call makes applies: `@`/`&` parameter types, `where` clauses, definiteness
# smileys and arity (#12963).

plan 12;

{
    my @a;
    throws-like { :(Int @a) := \((1, "x")) }, X::TypeCheck::Binding::Parameter,
        'a typed @ parameter checks its elements';
}

{
    my &c;
    throws-like { :(Callable &c) := \(5) }, X::TypeCheck::Binding::Parameter,
        'a & parameter wants a Callable';
}

{
    my $x;
    # rakudo 2026.09 dies here too, with an internal X::AdHoc; mutsu raises
    # the binder's X::TypeCheck::Binding::Parameter.
    dies-ok { :($x where * > 5) := \(1) }, 'a where clause is checked';
    :($x where * > 5) := \(7);
    is $x, 7, 'a passing where clause binds';
}

{
    my $a;
    throws-like { :(Int:D $a) := \(Int) }, X::Parameter::InvalidConcreteness,
        'a :D smiley refuses a type object';
}

{
    my ($a, $b);
    dies-ok { :($a, $b) := \(1) }, 'too few positionals die';
    dies-ok { :($a) := \(1, 2) }, 'too many positionals die';
    :($a, $b) := \(1, 2);
    is "$a $b", '1 2', 'a matching capture binds';
}

{
    my ($a, @tail);
    :($a, *@tail) := (1, 2, 3);
    is-deeply @tail, [2, 3], 'a slurpy takes the rest';
}

{
    my ($t, $m);
    :(:type($t), :motivation($m)) := (type => 'geek', motivation => '-Ofun');
    is "$t $m", 'geek -Ofun', 'renamed named parameters bind their variables';
}

{
    my $b = 1;
    my $a;
    :(:$a is raw) := \(:a($b));
    $a = 3;
    is $b, 3, 'an is raw named parameter aliases its static source';
}

{
    my @a;
    :(Int @a) := \(Array[Int].new(1, 2));
    is-deeply @a.List, (1, 2), 'a typed @ parameter binds a matching typed array';
}
