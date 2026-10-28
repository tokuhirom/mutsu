use Test;

plan 7;

# An unknown alphanumeric escape in an interpolating quote is a compile-time
# error: the whole unit is rejected, not just the statement that reaches it.
throws-like { EVAL 'my $s = "a\zb"; 1' }, X::Backslash::UnrecognizedSequence,
    'double-quoted string';
throws-like { EVAL 'qq:b/\z/' }, X::Backslash::UnrecognizedSequence,
    'qq:b';
throws-like { EVAL Q{my $x = "a\qq[b\z]"; 1} }, X::Backslash::UnrecognizedSequence,
    'nested inline quote';
throws-like { EVAL 'my $r = 1; "ok"; if 0 { say "x\Ay" }; 1' }, X::Backslash::UnrecognizedSequence,
    'a branch that never runs is still rejected';
throws-like { EVAL 'my $u = qq:to/END/;' ~ "\nbad \\z\nEND\n" ~ '1' }, X::Backslash::UnrecognizedSequence,
    'interpolating heredoc';

# Known escapes and non-word escapes are untouched.
is "a\x41\t|", "aA\t|", 'known escapes';
is qq[\qq[a\x41]], 'aA', 'nested quote with a known escape';
