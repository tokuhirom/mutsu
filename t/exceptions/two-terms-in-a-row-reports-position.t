use Test;
use lib 'roast/packages/Test-Helpers/lib';
use Test::Util;

# `PError::fatal_at` exists so a fatal parse error carries the failure position,
# and `parse_program` then reports `at <file>:<line>` plus the `------>` source
# echo. Five "Two terms in a row" sites still raised the position-less
# `PError::fatal` / `PError::fatal_with_exception`, so the diagnosis was only
# the bare message -- untriageable on a real file, and six ecosystem
# distributions failed with nothing but `Parse error: Confused. Two terms in a
# row`, each needing a hand prefix-bisect before its cause could even be named.
#
# Each case below buries the offending construct a few lines into the program,
# so the assertions distinguish a real position from a default of 1. Every one
# of the five is a genuine "Two terms in a row" in rakudo too.

plan 15;

sub reports-position($code, $line, $echo, $desc) {
    is_run(
        $code,
        {
            status => sub { 0 != $^a },
            err    => rx/'Two terms in a row'/,
        },
        "$desc: raises the diagnosis",
    );
    # `is_run` compiles from a temp file, so match the `:<line>` suffix rather
    # than a fixed file name.
    is_run(
        $code,
        { status => sub { 0 != $^a }, err => rx/"\nat " \N+ ":$line\n"/ },
        "$desc: carries the source position",
    );
    is_run(
        $code,
        { status => sub { 0 != $^a }, err => rx/'------>' \N* $echo/ },
        "$desc: echoes the offending line",
    );
}

# src/parser/stmt/modifier.rs -- a term after a `for` modifier's iterable.
reports-position(q:to/CODE/, 3, 'for', 'a term after a `for` iterable');
my $x = 1;
say $x;
.say for (1, 2, 3) "!";
CODE

# src/parser/stmt/modifier.rs -- a bare word starting the next line.
#
# Both implementations report line 4 (the line the second term is ON), but
# echo line 3 (the line that is actually missing its semicolon) -- #8329.
reports-position(q:to/CODE/, 4, '42 if 23', 'two terms across lines');
my $y = 1;
say $y;
42 if 23
is 50; 1
CODE

# src/parser/primary/ident/identifier_call.rs -- an identifier butted against a
# quote, `foo'bar'`.
reports-position(q:to/CODE/, 3, 'foo', 'an identifier butted against a quote');
my $z = 1;
say $z;
foo'bar';
CODE

# src/parser/primary/misc/reduction.rs -- a listop reduction with no space
# before its operand, `[+]@a`.
reports-position(q:to/CODE/, 3, 'say', 'a reduction butted against its operand');
my @a = 1, 2;
say 'x';
say [+]@a;
CODE

# src/parser/stmt/decl/has_decl.rs -- this one raised through
# `fatal_with_exception`, which had no position field at all, so it needed a new
# `fatal_with_exception_at` rather than just being switched to `fatal_at`.
reports-position(q:to/CODE/, 3, 'has', 'two attribute terms in a `has` declaration');
class Filler {
}
class C { has $.x $.y; }
CODE
