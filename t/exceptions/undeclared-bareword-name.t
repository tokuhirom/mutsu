use Test;

# An undeclared bareword term is a compile-time "Undeclared name"
# (X::Undeclared::Symbols), as in rakudo (#9768). It used to fall through to
# the run-time bareword lookup, whose last resort turned it into its own name
# as a Str, so `FooBarBaz.^name` answered `Str` and the program kept running.
# A compile-time error cannot be caught in-process, so the program runs in a
# child process.

plan 12;

sub run-snippet(Str:D $code) {
    my $proc = run $*EXECUTABLE.absolute, '-e', $code, :out, :err;
    ($proc.exitcode, $proc.out.slurp(:close), $proc.err.slurp(:close));
}

{
    my ($status, $out, $err) = run-snippet 'say FooBarBaz.^name; say "alive"';
    isnt $status, 0, 'an undeclared bareword term fails the program';
    is $out, '', '...before anything runs';
    like $err, /'Undeclared ' \w+ ':' \s+ 'FooBarBaz used at line 1'/,
        '...as an undeclared symbol naming the term and its line';
}

{
    my ($status, $out, $err) = run-snippet "say 1;\nmy \$x = 2;\nsay Nope;";
    isnt $status, 0, 'an undeclared term on a later line fails too';
    is $out, '', '...still before the earlier statements run';
    like $err, /'Nope used at line 3'/, '...reporting the line of the term';
}

{
    my ($status, $out, $err) = run-snippet 'sub f { Missing.new }; say "alive"';
    isnt $status, 0, 'an undeclared term inside a routine body is caught at compile time';
    like $err, /'Undeclared ' \w+ ':' \s+ 'Missing used at line 1'/, '...with the same message';
}

# Legitimate barewords are untouched.
{
    my ($status, $out, $err) = run-snippet q:to/END/;
        my %h = foo => 1, :bar(2);
        my @w = <baz qux>;
        class Later { ... }
        say Later.^name;
        class Later { }
        enum Color <Red Green>;
        constant Answer = 42;
        say %h<foo> ~ %h<bar> ~ @w[0] ~ Red ~ Answer;
        say Int.^name;
        say Str:D.^name;
        END
    is $status, 0, 'pair keys, angle words, declared types, enums and constants still work';
    is $out, "Later\n12bazRed42\nInt\nStr:D\n", '...with the right values';
    is $err, '', '...and nothing on stderr';
}

# EVAL keeps its own check.
throws-like 'FooBarBaz.^name', X::Undeclared::Symbols,
    'EVAL of an undeclared bareword term is X::Undeclared::Symbols';
