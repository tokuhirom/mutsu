use v6;
use Test;

# A statement whose last expression ends in a `{ ... }` block is self-terminating
# when that brace ends the line: `my @a = gather { ... }` followed by a line
# starting with `if` is two statements, not a modifier.
#
# mutsu decided that from the AST alone — "does the trailing expression contain a
# block" — which is also true of `@a = @a.grep({ ... })`, where the line ends in
# `)` and the statement is therefore NOT finished. The next line's `if` really is
# its modifier, as rakudo reads it:
#
#     @envelopes = @envelopes.grep({ ($_.id // -1) != $exclude-id })
#         if $exclude-id.defined;
#
# (App::Moneymoor, which writes ten of its modules that way.) The rule now reads
# the source text to see whether the `}` actually ends the line.

plan 10;

# The modifier attaches when the line ends in something other than `}`.
{
    my @a = 1, 2, 3;
    my $on = True;
    @a = @a.grep({ $_ != 2 })
        if $on;
    is-deeply @a, [1, 3], 'a modifier on the next line applies to a `.grep({...})` assignment';
}
{
    my @a = 1, 2, 3;
    my $on = False;
    @a = @a.grep({ $_ != 2 })
        if $on;
    is-deeply @a, [1, 2, 3], 'and a false condition leaves the assignment undone';
}
{
    my @seen;
    my $n = 0;
    @seen.push(
        do { $n++ }
    ) for 1, 2;
    is $n, 2, 'a `for` modifier after a multi-line argument list still loops';
}
{
    my @a = 1, 2, 3;
    my @kept = @a.map({ $_ * 2 })
        unless False;
    is-deeply @kept, [2, 4, 6], 'an `unless` modifier attaches the same way';
}

# A `}` that really does end the line still terminates the statement.
{
    my @a = gather { take 7 }
    if @a { pass 'a following `if` is its own statement after a `gather {...}` line' }
    is-deeply @a, [7], 'and the gather assignment kept its value unconditionally';
}
{
    my $x = do { 5 }
    unless $x { flunk 'unreachable' }
    is $x, 5, 'a `do {...}` line is self-terminating too';
}
{
    my $ran = False;
    sub f(@mask) {
        return 'early' if @mask.first: { !.defined }
        $ran = True;
        'late'
    }
    is f([1, 2]), 'late', 'a block-terminated modifier condition still ends the statement';
    ok $ran, 'so the following statement ran';
}

# A bare block statement is unaffected: a modifier never attaches across a newline.
{
    my $count = 0;
    {
        $count++;
    }
    if $count { pass 'a bare block keeps its `}`-ends-the-line reading' }
}
