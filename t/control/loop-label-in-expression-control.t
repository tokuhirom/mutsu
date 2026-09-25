use Test;

# `next` / `last` / `redo` used as an expression (inside parens, a ternary
# branch) keep their loop label, as the statement forms do. The label was
# dropped and left behind as a stray bareword, so the enclosing labeled loop
# failed to parse (Rakudo-Type-Introspection:
# `$WHO{$part}:exists ?? ... !! (next LEVEL)`).

plan 5;

my @seen;
LEVEL: for 1..2 -> $a {
    for 1..3 -> $b {
        $b == 2 ?? (next LEVEL) !! @seen.push("$a$b");
    }
}
is-deeply @seen, ['11', '21'], '(next LABEL) in a ternary branch skips to the outer loop';

@seen = ();
ROWS: for 1..2 -> $a {
    for 1..3 -> $b {
        $b == 2 ?? (last ROWS) !! @seen.push("$a$b");
    }
}
is-deeply @seen, ['11'], '(last LABEL) in a ternary branch leaves the outer loop';

@seen = ();
for 1..3 { $_ == 2 ?? (next) !! @seen.push($_) }
is-deeply @seen, [1, 3], 'unlabeled (next) still works';

@seen = ();
L: for 1..3 { @seen.push($_); ($_ == 2) && (next L); @seen.push("x$_") }
is-deeply @seen, [1, 'x1', 2, 3, 'x3'], '(next LABEL) after &&';

my %h = next => 1;
is %h<next>, 1, 'next => ... is still a pair key';
