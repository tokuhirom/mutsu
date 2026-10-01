use Test;

# `require Foo` with a literal target declares a stub package `Foo` in the
# lexical scope of the statement, while the program compiles. The name then
# resolves from the head of that scope on, and it stays resolvable when the
# load fails (#9873).

plan 13;

# A lookup before the statement already sees the stub.
is ::('PreRequireFoo9873').^name, 'PreRequireFoo9873',
    'the stub of a later require resolves before the statement';
try require PreRequireFoo9873;
is ::('PreRequireFoo9873').^name, 'PreRequireFoo9873',
    'the stub is still there after the failed load';

# The original repro: `try` swallows the failure, the stub remains.
try require MissingRequireTop9873;
is ::('MissingRequireTop9873').^name, 'MissingRequireTop9873',
    'try require of a missing module leaves a stub package';
isa-ok $!, X::CompUnit::UnsatisfiedDependency, 'the load itself still fails';

# A qualified name is a stub too.
try require Missing9873::Inner::Mod;
is ::('Missing9873::Inner::Mod').^name, 'Missing9873::Inner::Mod',
    'a qualified target gets a stub';

# The bare name is a type from the statement on.
try require BareMissing9873;
is BareMissing9873.^name, 'BareMissing9873', 'the stub is usable as a bareword';

# The require sits inside a larger expression.
my $loaded = so try require InExprMissing9873;
is $loaded, False, 'the failed load is falsy';
is ::('InExprMissing9873').^name, 'InExprMissing9873',
    'a require nested in an expression declares its stub';

# The stub is lexical: it does not escape the block that holds the require.
{
    try require MissingRequireScoped9873;
    is ::('MissingRequireScoped9873').^name, 'MissingRequireScoped9873',
        'the stub is visible inside its block';
}
is ::('MissingRequireScoped9873').^name, 'Failure',
    'the stub does not escape the block';

# A computed target names nothing until it runs: no stub.
try require ::('MissingRequireDynamic9873');
is ::('MissingRequireDynamic9873').^name, 'Failure',
    'a computed target leaves no stub';

# A routine declares the stubs of its own requires on entry.
sub load-it {
    try require MissingInSub9873;
    ::('MissingInSub9873').^name
}
is load-it(), 'MissingInSub9873', 'a routine body declares its stub';

# The stub never shadows a module that does load.
use lib 'roast/packages/S11-modules/lib';
{
    my $m = (require InnerModule);
    is $m.^name, 'InnerModule', 'a require that succeeds still returns the real module';
}
