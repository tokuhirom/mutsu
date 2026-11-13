use Test;

# An `EVAL` that declares a sub textually identical to one of the program's is
# a new routine over the EVAL's own scope: its free variables are the EVAL's,
# also when it is called from a closure `map` runs (#17644).

plan 3;

my $effects;
sub tick { $effects++; 1 }

my $result = EVAL q:to/CODE/;
    my $effects = 0;
    sub tick { $effects++; 1 }
    ({ tick() },).map({ $_() }).eager;
    tick();
    $effects
    CODE
is $result, 2, 'the EVAL sub writes the EVAL lexical';
nok $effects.defined, 'the program lexical is untouched';
tick();
is $effects, 1, 'the program sub still writes the program lexical';
