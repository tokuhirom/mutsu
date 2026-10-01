use Test;

plan 6;

# A statement-modifier `for` opens no block: `temp` restores at the enclosing
# block's exit, not after each iteration.
{
    my $a = 1;
    my $in;
    { temp $a = 5 for ^1; $in = $a }
    is $in, 5, 'temp behind a for modifier is live for the rest of the block';
    is $a, 1, 'temp behind a for modifier is restored at the block exit';
}

# A `let` behind an `if` modifier is rolled back when the enclosing block
# fails, with a constant or a run-time condition.
{
    my $e = 1; { let $e = 9 if 1; Nil }
    is $e, 1, 'let behind a constant-true if modifier rolls back on Nil';
    my $c0 = 1;
    my $f = 1; { let $f = 9 if $c0; Nil }
    is $f, 1, 'let behind a run-time if modifier rolls back on Nil';
    my $c = 1; { (let $c = 9) if 1; Nil }
    is $c, 1, 'parenthesised let behind an if modifier rolls back on Nil';
    my $g = 1; { let $g = 9 if 1; 5 }
    is $g, 9, 'let behind an if modifier is kept when the block succeeds';
}
