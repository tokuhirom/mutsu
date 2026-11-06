use Test;

# The "Your printf-style directives specify ..." message is word-wrapped as a
# whole, the format echoed inline (Type/independent-routines.rakudoc).
plan 4;

sub msg(Str $fmt, *@args) { try sprintf($fmt, |@args); $!.message }

is msg('%d %d', 1),
    "Your printf-style directives specify 2 arguments, but 1 argument was\nsupplied to format '%d %d'. Are you using an interpolated '\$'?",
    'one argument supplied';
is msg('Product Ab-%x-42; cost: $%d', "30"),
    "Your printf-style directives specify 2 arguments, but 1 argument was\nsupplied to format 'Product Ab-%x-42; cost: \$%d'. Are you using an\ninterpolated '\$'?",
    'the hint wraps onto its own line';
is msg('a very long format string that goes on and on %d %d %d %d and on', 1),
    "Your printf-style directives specify 4 arguments, but 1 argument was\nsupplied to format 'a very long format string that goes on and on %d %d\n%d %d and on'. Are you using an interpolated '\$'?",
    'a long format wraps inside its own text';
is msg('%d', 1, 2, 3),
    "Your printf-style directives specify 1 argument, but 3 arguments were\nsupplied to format '%d'.",
    'more arguments than directives has no hint';
