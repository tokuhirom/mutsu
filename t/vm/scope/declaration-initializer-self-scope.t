use Test;

plan 24;

# A declared variable is already in scope for its own initializer (#9770).
# Reading it directly there is a compile-time error; a nested code object
# there sees the new (not yet initialized) binding, never a shadowed outer one.

for 'my $x = 5; { my $x = $x + 1 }', 'my $z = $z', 'my Int $x = $x',
    'my $x := $x', 'state $x = $x', 'my $x = "a$x"', 'my $x = [$x]',
    'my $f = * + $f' -> $code {
    throws-like $code, X::Syntax::Variable::Initializer, "rejected: $code";
}

throws-like 'my %h is default(%h<foo>)', X::Syntax::Variable::Initializer,
    name => '%h', 'a trait argument counts as the initializer';
throws-like 'my @foo := 1..3, (@foo Z+ 100)', X::Syntax::Variable::Initializer,
    name => '@foo', message => 'Cannot use variable @foo in declaration to initialize itself',
    'the error names the variable';

{
    my $x = 5;
    {
        my $x = do { $x };
        is $x.raku, 'Any', 'a do block in the initializer sees the new binding';
    }
    is $x, 5, 'the outer binding is untouched';
    {
        my $x = sub { $x };
        ok $x() ~~ Sub, 'a closure in the initializer captures the new binding';
    }
    {
        my \x = $x;
        is x, 5, 'a sigilless declaration may read the same-named $ variable';
    }
}

{
    my $x = 5;
    my @seen;
    for ^2 { my $x = do { $x }; @seen.push($x.raku) }
    is @seen.join(' '), 'Any Any', 'a loop-body declaration shadows from its first iteration';
}

{
    my $*X = 5;
    sub f { my $*X = ($*X // 0) + 1; $*X }
    is f(), 1, 'a dynamic declaration reads its own fresh binding';
    is $*X, 5, 'and leaves the caller\'s binding alone';
}

# The `where` clause of a declaration is parsed before the variable exists, so
# reading the variable there names no binding -- a compile-time error (rakudo
# 2026.07 says X::Undeclared, 2026.09 X::Syntax::Variable::Initializer; both are
# X::Comp) -- unless an enclosing scope declares one, or the clause itself does.
for 'my $x where { $x > 0 } = 5', 'my Int $x where { $x > 0 } = 5',
    'my @a where { @a.elems } = 1, 2', 'my $x where $x > 0 = 5' -> $code {
    throws-like $code, X::Comp, "rejected: $code";
}

is EVAL(q/my $x where { $_ > 0 } = 5; $x/), 5, 'a where block that reads the topic is fine';
is EVAL(q/my $x where { my $x = 3; $x > 0 } = 5; $x/), 5,
    'a where block that declares its own variable is fine';
is EVAL(q/my $x where { True } = 5; $x/), 5, 'a where block that reads nothing is fine';
