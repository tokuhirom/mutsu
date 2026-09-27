use Test;

plan 4;

my $args = 'outer';
my &wrapped = -> |args {
    is args[0], 42, 'the sigilless parameter remains the call capture';
    $args = args;
    is $args[0], 42, 'the same-named scalar resolves to the enclosing lexical';
};

wrapped(42);
is $args[0], 42, 'the enclosing scalar keeps the closure write';
is $args.elems, 1, 'the enclosing scalar was assigned one captured argument';
