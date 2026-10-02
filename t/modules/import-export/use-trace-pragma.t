use Test;

# `use trace` is a lexical pragma: every statement parsed while it is in effect
# writes "<number> (<file> line <line>)\n<source>\n" to stderr just before it
# runs. The numbers are rakudo's `$*STATEMENT_ID`, which counts every attempt to
# parse a `statement` -- see src/parser/stmt/trace.rs for the rules.
#
# Every expectation below was taken from `raku` (2026.09) and the file passes
# under it unchanged.

plan 29;

sub trace-of(Str $code --> Str) {
    my $proc = run $*EXECUTABLE, '-e', $code, :out, :err;
    $proc.out.slurp(:close);
    $proc.err.slurp(:close)
}

is trace-of('use trace; say 1; say 2; say 3'), q:to/END/,
    2 (-e line 1)
    say 1
    3 (-e line 1)
    say 2
    4 (-e line 1)
    say 3
    END
    'each statement is traced with its number and its source text';

is trace-of('use trace; if 1 { say 1 }; my $x = 2 + 3; say $x'), q:to/END/,
    3 (-e line 1)
    say 1
    5 (-e line 1)
    my $x = 2 + 3
    6 (-e line 1)
    say $x
    END
    'the failed attempt at a block closing brace takes a number (4 here)';

is trace-of('sub g { say 1 }; use trace; say 3'), q:to/END/,
    5 (-e line 1)
    say 3
    END
    'statements before the pragma are counted';

is trace-of('use v6.d; use trace; say 1'), q:to/END/,
    2 (-e line 1)
    say 1
    END
    'a language version that opens the unit is not a numbered statement';

# --- what is and is not printed ---------------------------------------------

is trace-of('use trace; my $x = 1; if $x { say 1 }; unless $x { say 2 }; if 1 { say 3 }; '
        ~ 'if 0 { say 4 }; unless 0 { say 5 }; unless 1 { say 6 }; if True { say 7 }; say 8'),
    q:to/END/,
    2 (-e line 1)
    my $x = 1
    3 (-e line 1)
    if $x { say 1 }
    4 (-e line 1)
    say 1
    6 (-e line 1)
    unless $x { say 2 }
    10 (-e line 1)
    say 3
    16 (-e line 1)
    say 5
    22 (-e line 1)
    say 7
    24 (-e line 1)
    say 8
    END
    'an if/unless on a constant condition is folded away with its trace; a variable one is printed';

is trace-of('use trace; if 0 { say 1 } elsif 1 { say 2 }; say 3'), q:to/END/,
    2 (-e line 1)
    if 0 { say 1 } elsif 1 { say 2 }
    5 (-e line 1)
    say 2
    7 (-e line 1)
    say 3
    END
    'a constant-false if with an elsif chain is left to run time';

is trace-of('use trace; if 1 { say 1 } elsif 0 { say 2 } else { say 3 }; say 4'), q:to/END/,
    3 (-e line 1)
    say 1
    9 (-e line 1)
    say 4
    END
    'a constant-true if is folded whatever follows it';

is trace-of('use trace; for 1..2 { say $_ }; my $i = 0; while $i < 2 { $i++ }; say $i'), q:to/END/,
    2 (-e line 1)
    for 1..2 { say $_ }
    3 (-e line 1)
    say $_
    3 (-e line 1)
    say $_
    5 (-e line 1)
    my $i = 0
    6 (-e line 1)
    while $i < 2 { $i++ }
    7 (-e line 1)
    $i++
    7 (-e line 1)
    $i++
    9 (-e line 1)
    say $i
    END
    'loops are printed, and so is their body each time it runs';

is trace-of('use trace; given 2 { when 1 { say "one" }; default { say "other" } }; say 9'), q:to/END/,
    2 (-e line 1)
    given 2 { when 1 { say "one" }; default { say "other" } }
    3 (-e line 1)
    when 1 { say "one" }
    6 (-e line 1)
    default { say "other" }
    7 (-e line 1)
    say "other"
    10 (-e line 1)
    say 9
    END
    'given/when/default are statements too';

is trace-of('use trace; sub f($x) { say $x }; f(1); my $c = -> $y { say $y }; $c(2)'), q:to/END/,
    2 (-e line 1)
    sub f($x) { say $x }
    5 (-e line 1)
    f(1)
    3 (-e line 1)
    say $x
    6 (-e line 1)
    my $c = -> $y { say $y }
    9 (-e line 1)
    $c(2)
    7 (-e line 1)
    say $y
    END
    'numbers are fixed when parsing, so a body prints its own number when it runs';

is trace-of('use trace; class A { has $.x = 5; method m { say $.x } }; A.new.m'), q:to/END/,
    2 (-e line 1)
    class A { has $.x = 5; method m { say $.x } }
    3 (-e line 1)
    has $.x = 5
    4 (-e line 1)
    method m { say $.x }
    8 (-e line 1)
    A.new.m
    5 (-e line 1)
    say $.x
    END
    'declarations in a class body are traced';

is trace-of('use trace; EVAL q[say 1]; EVAL q[use trace; say 2]'), q:to/END/,
    2 (-e line 1)
    EVAL q[say 1]
    3 (-e line 1)
    EVAL q[use trace; say 2]
    2 (EVAL_1 line 1)
    say 2
    END
    'the pragma does not reach an EVAL, which is a compilation unit of its own';

# --- the statement number ----------------------------------------------------

is trace-of('use trace; my $a = (1, 2); my @b = [1, [2]]; say @b[0]; my %h = a => 1; '
        ~ 'say %h{"a"}; say %h<a>; say "@b[0]"'),
    q:to/END/,
    2 (-e line 1)
    my $a = (1, 2)
    5 (-e line 1)
    my @b = [1, [2]]
    10 (-e line 1)
    say @b[0]
    13 (-e line 1)
    my %h = a => 1
    14 (-e line 1)
    say %h{"a"}
    17 (-e line 1)
    say %h<a>
    18 (-e line 1)
    say "@b[0]"
    END
    'the contents of (...), [...] and a subscript are statement lists of their own';

is trace-of('use trace; my $a = (); my @b = []; sub f(*@a) { 1 }; f(1, 2); say 1'), q:to/END/,
    2 (-e line 1)
    my $a = ()
    3 (-e line 1)
    my @b = []
    4 (-e line 1)
    sub f(*@a) { 1 }
    7 (-e line 1)
    f(1, 2)
    5 (-e line 1)
    1
    8 (-e line 1)
    say 1
    END
    'an empty list and a call argument list take no number';

is trace-of('use trace; FOO: for 1..2 { last FOO }; say 5'), q:to/END/,
    3 (-e line 1)
    FOO: for 1..2 { last FOO }
    4 (-e line 1)
    last FOO
    6 (-e line 1)
    say 5
    END
    'a statement label is an attempt of its own (the header carries the second)';

is trace-of('use trace; say 1;; say 2;;; say 3'), q:to/END/,
    2 (-e line 1)
    say 1
    4 (-e line 1)
    say 2
    7 (-e line 1)
    say 3
    END
    'every empty statement between semicolons takes a number';

is trace-of('use trace; proto p($x) {*}; multi p(Int $x) { say "int" }; p(1)'), q:to/END/,
    2 (-e line 1)
    proto p($x) {*}
    3 (-e line 1)
    multi p(Int $x) { say "int" }
    6 (-e line 1)
    p(1)
    4 (-e line 1)
    say "int"
    END
    'the {*} body of a proto is not a statement list';

is trace-of('use trace; sub f {...}; try { f() }; say $!.message'), q:to/END/,
    2 (-e line 1)
    sub f {...}
    5 (-e line 1)
    try { f() }
    6 (-e line 1)
    f()
    3 (-e line 1)
    ...
    8 (-e line 1)
    say $!.message
    END
    'a stub body is traced and still dies with "Stub code executed"';

is trace-of('use trace; BEGIN { say 1 }; END { say 3 }; say 2'), q:to/END/,
    3 (-e line 1)
    say 1
    2 (-e line 1)
    BEGIN { say 1 }
    5 (-e line 1)
    END { say 3 }
    8 (-e line 1)
    say 2
    6 (-e line 1)
    say 3
    END
    'phaser bodies are traced when they run (BEGIN at compile time)';

is trace-of('use trace; try { die "x"; CATCH { default { say "c" } } }; say 4'), q:to/END/,
    2 (-e line 1)
    try { die "x"; CATCH { default { say "c" } } }
    3 (-e line 1)
    die "x"
    5 (-e line 1)
    default { say "c" }
    6 (-e line 1)
    say "c"
    10 (-e line 1)
    say 4
    END
    'a CATCH block inside try is numbered after the statements before it';

# --- lexical scope -----------------------------------------------------------

is trace-of('use trace; say 1; { no trace; say 2 }; say 3; no trace; say 4; use trace; say 5'),
    q:to/END/,
    2 (-e line 1)
    say 1
    3 (-e line 1)
    { no trace; say 2 }
    7 (-e line 1)
    say 3
    11 (-e line 1)
    say 5
    END
    'no trace ends the pragma in its block, and neither it nor use trace is printed';

is trace-of('{ use trace; say 1 }; say 2'), q:to/END/,
    3 (-e line 1)
    say 1
    END
    'the pragma ends with the block it was used in';

# --- the text of a statement --------------------------------------------------

is trace-of(q:to/CODE/), q:to/END/,
    use trace; say 1;
    say
      2 +
      3;  # a trailing comment
    say "a;b"; say 5 # no terminator
    CODE
    2 (-e line 1)
    say 1
    3 (-e line 2)
    say
      2 +
      3
    4 (-e line 5)
    say "a;b"
    5 (-e line 5)
    say 5
    END
    'a multi-line statement is printed as written, from its start line, without its terminator or a trailing comment';

# --- what the pragma does not change -------------------------------------------

{
    my $code = q:to/CODE/;
        use trace;
        my class Cap {
            has @.got;
            method print(*@a) { @!got.push: @a.join; True }
            method flush { True }
        }
        my $cap = Cap.new;
        {
            my $*ERR = $cap;
            say "inside";
        }
        say "captured: ", $cap.got.elems;
        CODE
    my $proc = run $*EXECUTABLE, '-e', $code, :out, :err;
    my $out = $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);
    is $out, "inside\ncaptured: 0\n", 'the trace is not written through the $*ERR variable';
    ok $err.contains("15 (-e line 10)\nsay \"inside\"\n"),
        'it reaches the real standard error even when $*ERR is rebound';
}

{
    my $proc = run $*EXECUTABLE, '-e',
        'use trace; role R { method m {...} }; class A does R { method m { 1 } }; say A.new.m',
        :out, :err;
    is $proc.out.slurp(:close), "1\n", 'a stubbed role method is still recognised as a stub';
    $proc.err.slurp(:close);
}

{
    my $proc = run $*EXECUTABLE, '-e',
        'use trace; sub f($x) { $x > 1 ?? "big" !! "small" }; say f(2), f(0); '
            ~ 'say (1, 2, 3).map({ $_ * 2 }).join(",")',
        :out, :err;
    is $proc.out.slurp(:close), "bigsmall\n2,4,6\n", 'programs compute the same results with the pragma on';
    $proc.err.slurp(:close);
}

# The statements of a regex code block are a statement list of their own: one
# number per statement, plus the failed attempt at its closing brace. Only the
# header of the statement after the regex is compared here.
{
    sub header-of-say(Str $code --> Str) {
        my @lines = trace-of($code).lines;
        @lines[(@lines.first(:k, 'say 5') // 0) - 1]
    }
    my @cases =
        '/ a { 1 } /'          => 5,
        '/ a { 1; 2 } /'       => 6,
        '/ a { if 1 { 2 } } /' => 7,
        '/ a {} /'             => 3,
        '/ a <?{ 1 }> /'       => 5,
        '/ a <{ "a" }> /'      => 5,
        'm/ a { 1 } /'         => 5,
        'rx/ a { 1 } /'        => 5;
    my @got = @cases.map({ header-of-say("use trace; my \$a = \"a\" ~~ {.key}; say 5") });
    is-deeply @got, [@cases.map({ "{.value} (-e line 1)" })],
        'the statements of a regex code block are numbered';
    is header-of-say('use trace; my token t { x { 3 } }; say 5'), '5 (-e line 1)',
        '... in a token declaration too';
}

# vim: expandtab shiftwidth=4
