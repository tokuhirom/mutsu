use Test;

plan 10;

# A `for` STATEMENT MODIFIER is not a block: its body is evaluated in the
# enclosing scope, so a placeholder written there belongs to the *enclosing*
# block, not to the loop. A `for` BLOCK is its own placeholder scope
# (`for @a { $^x }` gives the loop the parameter), and the AST spells both as
# `Stmt::For` — hence the `is_statement_modifier` flag.

{
    my @seen;
    my $f = { @seen.push($^b) for (1, 2) };
    $f(42);
    is @seen, (42, 42), 'a placeholder in a `for` modifier body is the enclosing block parameter';
}

{
    my $f = { $^b for (1, 2) };
    is $f.arity, 1, '...so the block has arity 1';
    is $f.count, 1, '...and count 1';
}

# The shape Digest::MD5 / Digest::RIPEMD build their message block with.
{
    my $build = {
        $^b.push($_) for (10, 20, 30);
        $b.push(40);
        $b;
    };
    is $build(my @acc), (10, 20, 30, 40),
        'the plain spelling `$b` refers back to the `$^b` used in the modifier body';
}

# The block form keeps giving the LOOP the parameter.
{
    my @doubled;
    for (7, 8) { @doubled.push($^x * 2) }
    is @doubled, (14, 16), 'a `for` block body still owns its own placeholders';
}

{
    my $f = { my @r; for (1, 2) { @r.push($^y) }; @r };
    is $f(9), (1, 2), '...even when nested in a placeholder block (bound to the loop value)';
    is $f.arity, 0, '...and the enclosing block does not claim it';
}

# `while` / `until` modifiers behave the same way.
{
    my $i = 0;
    my @seen;
    my $f = { @seen.push($^b) while $i++ < 2 };
    $f(42);
    is @seen, (42, 42), 'a placeholder in a `while` modifier body is the enclosing parameter';
}

# A placeholder declares its plain spelling as a lexical of the block.
{
    my $f = { my $z = $^b; $z + $b };
    is $f(21), 42, '`my $z = $^b` makes the later `$b` declared';
}

# ...and using the plain spelling BEFORE the placeholder is still an error.
throws-like 'my $f = { say $b; say $^b }; $f(1)', X::Undeclared,
    'a bare `$b` in a statement before its `$^b` is undeclared';
