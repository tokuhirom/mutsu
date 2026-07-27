use Test;

# A block-form given/when/default body is a lexical scope. A statement
# modifier is not: its declaration belongs to the surrounding statement list.

plan 16;

{
    my $x = 1;
    given 1 { my $x = 5; is $x, 5, 'given sees its scalar shadow' }
    is $x, 1, 'given scalar shadow does not clobber outer lexical';
}

{
    my @x = 1, 2;
    given 1 { my @x = 5, 6; is @x.join(','), '5,6', 'given sees its array shadow' }
    is @x.join(','), '1,2', 'given array shadow does not clobber outer lexical';
}

{
    my %x = outer => 1;
    given 1 { my %x = inner => 5; is %x<inner>, 5, 'given sees its hash shadow' }
    is %x<outer>, 1, 'given hash shadow does not clobber outer lexical';
}

{
    my $x = 1;
    given 5 -> $x { is $x, 5, 'given pointy parameter shadows inside body' }
    is $x, 1, 'given pointy parameter stops shadowing after body';
}

given 5 -> int $native {
    is $native, 5, 'native pointy parameter is initialized in its lexical container';
}

{
    my $x = 1;
    given 2 {
        when 2 { my $x = 9; is $x, 9, 'when sees its scalar shadow' }
    }
    is $x, 1, 'when scalar shadow does not clobber outer lexical';
}

{
    my $x = 1;
    given 3 {
        default { my $x = 9; is $x, 9, 'default sees its scalar shadow' }
    }
    is $x, 1, 'default scalar shadow does not clobber outer lexical';
}

{
    (my $modifier = 9) given 2;
    is $modifier, 9, 'given statement modifier declaration remains enclosing-scoped';
}

{
    my $outer = 1;
    given 2 { $outer = 9 }
    is $outer, 9, 'plain assignment to an outer lexical still survives given';
}

given 'a' -> $_ is copy { }
is-deeply (S:g/FAIL// with 'foo'), 'foo',
    'expression-form with updates a pre-existing topic local slot';
