use Test;

# A statement modifier (`if` / `unless`) whose condition ends in a `}` block —
# e.g. a colon-block method call `@a.grep: { ... }` — is self-terminating at end
# of line, exactly like any block statement. A statement on the next line is a
# new statement, not "two terms in a row". (Terminal::UI::Pane regression:
#   die "..." if @args.grep: { $_ ~~ Str && /\e/ }
#   my $i = @!lines.elems;
# )

plan 6;

lives-ok {
    EVAL q:to/CODE/;
        my @a = 1, 2, 3;
        say 1 if @a.grep: { $_ > 1 }
        my $i = @a.elems;
        CODE
}, '`if` condition ending in a colon-block self-terminates before a new statement';

lives-ok {
    EVAL q:to/CODE/;
        my @a = 1, 2, 3;
        say 1 unless @a.map: { $_ }
        my $i = @a.elems;
        CODE
}, '`unless` condition ending in a colon-block self-terminates';

lives-ok {
    EVAL q:to/CODE/;
        my @a = 1, 2, 3;
        my $x = 0;
        $x = 9 if @a.first: { $_ == 2 }
        my $y = 1;
        CODE
}, 'the modified statement still runs after a block-ending condition';

# The condition is still evaluated correctly.
my @a = 1, 2, 3;
my $ran = 0;
$ran = 1 if @a.grep: { $_ > 2 }
my $after = 42;
is $ran, 1, 'the modifier fires when the block condition is truthy';
is $after, 42, 'the following statement is parsed and run';

# A genuine "two terms in a row" (non-block condition) is still a syntax error.
throws-like qq:to/CODE/, X::Syntax::Confused,
    my \@b = 1, 2;
    say 1 if \@b.elems
    say 2
    CODE
    'a non-block condition followed by a bare statement is still Confused';
