use Test;

plan 6;

# `else if` is a C-ism; Raku spells it `elsif` and rejects `else if` with a
# dedicated X::Syntax::Malformed::Elsif diagnostic.
throws-like
    'if 10 > 5 { say "a" } else if 10 == 5 { say "b" } else { say "c" }',
    X::Syntax::Malformed::Elsif,
    'else if is rejected with X::Syntax::Malformed::Elsif';

throws-like
    'if 1 { } else if 2 { }',
    X::Syntax::Malformed::Elsif,
    message => /elsif/,
    'diagnostic message mentions elsif';

# `else if` after an elsif chain is still caught.
throws-like
    'if 0 { } elsif 0 { } else if 1 { }',
    X::Syntax::Malformed::Elsif,
    'else if after an elsif chain is rejected';

# Legitimate spellings keep working.
lives-ok
    { EVAL 'my $x = 3; if $x == 1 { } elsif $x == 3 { } else { }' },
    'if/elsif/else parses fine';

is
    (EVAL 'my $x = 3; if $x == 1 { "one" } elsif $x == 3 { "three" } else { "other" }'),
    'three',
    'elsif chain selects the right branch';

# A bare identifier starting with "if" inside the else block is not mistaken
# for `else if`.
is
    (EVAL 'my $ifx = 9; if 0 { 1 } else { $ifx }'),
    9,
    'an else block referencing an "if"-prefixed name is fine';
