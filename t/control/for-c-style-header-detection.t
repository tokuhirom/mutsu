use Test;

# The obsolete C-style `for (init; test; step)` is the header shape
# `( EXPR? ; EXPR? ; EXPR? )`, recognised with the expression parser as
# rakudo does. A `#` inside a quote (`q<[a]#>`) is no comment, and a single
# `;` is a semicolon list, not a C-style loop. CSS::Specification::Compiler's
# t/compile.t iterates `for ( ..., q<[<generic-voice> | <specific-voice> ]#>, ... )`
# with `;`s in the loop body, and mutsu rejected it. Checked against raku.

plan 5;

my @seen;
for (1, q<[a]#>, 2) { my $x = $_; @seen.push: $x; }
is-deeply @seen, [1, '[a]#', 2], 'a # inside a quote does not end the list';

@seen = ();
for (1; 2) { @seen.push: $_ }
is-deeply @seen, [1, 2], 'a single ; is a semicolon list';

throws-like 'for (my $i = 0; $i < 3; $i++) { }', X::Obsolete,
    message => / 'C-style "for (;;)" loop' /, 'the C-style header is still obsolete';
throws-like 'for (;;) { last }', X::Obsolete, 'an empty C-style header too';

@seen = ();
for (1, # a comment with ; in it
     2) { @seen.push: $_ }
is-deeply @seen, [1, 2], 'a ; inside a comment is prose';
