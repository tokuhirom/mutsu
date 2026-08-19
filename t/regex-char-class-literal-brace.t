use Test;

# A literal `{`, `(`, `)`, `}`, or `[` inside an enumerated character class
# (`<[...]>`) is an ordinary class member in Raku regex syntax — char classes
# have no brace/paren nesting. `scan_angle_assertion_body()` in
# src/runtime/regex_parse_core.rs used to keep bumping its brace/paren depth
# counters for these characters even while inside a `[...]` class, so the
# scan ran past the assertion's real closing `>` and silently swallowed
# everything after it (see todo/tickets, now retired, for the reduced case:
# `Template::Jinja2`'s lexer grammar was entirely unusable because of this).

plan 15;

is ('Hello World' ~~ / <-[{]>+ /).Str, 'Hello World', 'literal { in negated class does not swallow the rest of the regex';
is ('Hello World' ~~ / <-[(]>+ /).Str, 'Hello World', 'literal ( in negated class does not swallow the rest of the regex';
is ('Hello World' ~~ / <-[}{]>+ /).Str, 'Hello World', 'literal }{ pair in negated class works';
is ('Hello World' ~~ / <-[{]> .+ /).Str, 'Hello World', 'atoms after the class are not swallowed';
is ('{{{x' ~~ / <[{]>+ /).Str, '{{{', 'positive class of literal { matches a run of braces';
is ('Hello World' ~~ / <-[[]>+ /).Str, 'Hello World', 'literal [ in negated class does not swallow the rest of the regex';
is ('Hello World' ~~ / <-[)]>+ /).Str, 'Hello World', 'a lone literal ) also works (already worked before the fix)';

# The bracket-escape forms that legitimately nest a `[` inside a class must
# keep working: \c[...], \C[...], \x[...], \X[...].
is ('a' ~~ / <[\c[LATIN SMALL LETTER A]]>+ /).Str, 'a', 'named-char-in-class escape \\c[...] still nests correctly';
is ("\x263A" ~~ / <[\x[263A]]>+ /).Str, "\x263A", 'hex-char-in-class escape \\x[...] still nests correctly';

# A grammar built on a class of "everything but an opening delimiter" is the
# idiomatic template/config-lexer pattern this bug broke end to end.
grammar TextRun {
    token TOP   { <chunk>* }
    token chunk { <-[{]>+ || '{' }
}
my $m = TextRun.parse('Hello {x} World');
ok $m.defined, 'grammar TOP over <-[{]>+ chunks parses the whole string';
is +$m<chunk>, 3, 'text-then-brace-delimited-text produces the expected chunk count';
is ~$m<chunk>[0], 'Hello ', 'first chunk is the run of plain text before the brace';

# Compound char classes: whitespace may separate a bracket group from a
# following +[...]/-[...] continuation (parser-level delimiter scanning).
is ('xa' ~~ / <[x] + [{]>+ /).Str, 'x', 'whitespace around a compound +[ continuation is allowed';
is ('xa' ~~ / <[x]+[y]>+ /).Str, 'x', 'compound +[ continuation with no whitespace still works';
is ('b' ~~ / <[a..z] -[aeiou]> /).Str, 'b', 'compound -[ subtraction still works (regression guard)';
