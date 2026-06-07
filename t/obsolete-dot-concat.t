use Test;
plan 9;

# Perl 5's `.` string-concatenation operator is obsolete in Raku (use `~`).
# A method call may have whitespace before the name (`$x . uc`), but a dot
# followed by whitespace and then a term that can never be a method name
# (string literal, number, or sigiled variable) is the obsolete concat.

for '"a" . "b"', 'my ($a, $b); $a . $b', 'my $a = 1; $a . 2',
    'my @a; @a . "x"', 'my %h; %h . "x"'
    -> $code {
    throws-like $code, X::Obsolete, "$code throws X::Obsolete";
}

# Valid method calls (with or without space before the dot, and indirect
# method calls without a space after the dot) must keep working.
is ("x" . uc), "X", 'space before dot + identifier is a method call';
is ("hello".chars), 5, 'no-space method call still works';

my $a = "abc";
is ($a.uc), "ABC", 'indirect-free method call works';

# `..` range is not affected by the obsolete-concat detection.
is (1 .. 3).elems, 3, 'range operator unaffected';
