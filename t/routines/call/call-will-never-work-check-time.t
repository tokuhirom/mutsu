use Test;

# #18874: a call rakudo's optimizer can prove will never bind is a CHECK-time
# compile error: nothing in the unit runs, and the error points at the call.
# Expected texts are rakudo 2026.11's.

plan 24;

sub run-code(Str $code) {
    my $p = run $*EXECUTABLE, '-e', $code, :out, :err;
    my $out = $p.out.slurp(:close);
    my $err = $p.err.slurp(:close).trim-trailing;
    ($out, $err, $p.exitcode)
}

# The report up to its snippet is rakudo's. The snippet marks the call:
# rakudo writes `<HERE>` into a window of the line, mutsu draws a caret under
# the whole line (as for every compile error), so both are read back as the
# call's column in its source line.
sub marked-column(@lines, Str $code) {
    my $at = @lines.first(*.starts-with('------>'), :k) // return Nil;
    my $line = @lines[$at - 1].match(/':' (\d+) $/)[0].Int;
    my $source = $code.lines[$line - 1];
    with @lines[$at].index('<HERE>') -> $here {
        return $source.index(@lines[$at].substr($here + 6));
    }
    (@lines[$at + 1] // '').index('^') - 7
}

sub refuted(Str $code, Str $expected, Str $desc) {
    my ($out, $err, $exit) = run-code($code);
    my @want = $expected.lines;
    my @got = $err.lines;
    my $head = @want.first(*.starts-with('------>'), :k);
    subtest $desc => {
        plan 4;
        is $out, '', 'nothing ran';
        is @got[^$head].join("\n"), @want[^$head].join("\n"), 'the compile error';
        is marked-column(@got, $code), marked-column(@want, $code), 'it points at the call';
        is $exit, 1, 'exit code 1';
    }
}

refuted q[say 1; sub f(Int $x) {}; f("a")],
    "===SORRY!=== Error while compiling -e\nCalling f(Str) will never work with declared signature (Int \$x)\nat -e:1\n------> say 1; sub f(Int \$x) \{}; <HERE>f(\"a\")",
    'a literal of the wrong type';
refuted q[say 1; sub f(Int $x) {}; f "a"],
    "===SORRY!=== Error while compiling -e\nCalling f(Str) will never work with declared signature (Int \$x)\nat -e:1\n------> say 1; sub f(Int \$x) \{}; <HERE>f \"a\"",
    'a listop call';
refuted qq[say 1;\nsub f(Int \$x) \{}\nsub g \{ f("a") }],
    "===SORRY!=== Error while compiling -e\nCalling f(Str) will never work with declared signature (Int \$x)\nat -e:3\n------> sub g \{ <HERE>f(\"a\") }",
    'a call inside another routine that never runs';
refuted q[say 1; sub g($a, $b) {}; g(1)],
    "===SORRY!=== Error while compiling -e\nCalling g(Int) will never work with declared signature (\$a, \$b)\nat -e:1\n------> say 1; sub g(\$a, \$b) \{}; <HERE>g(1)",
    'too few arguments';
refuted q[say 1; sub f(Num $x) {}; f(1, 2.5)],
    "===SORRY!=== Error while compiling -e\nCalling f(Int, Rat) will never work with declared signature (Num \$x)\nat -e:1\n------> say 1; sub f(Num \$x) \{}; <HERE>f(1, 2.5)",
    'too many arguments';
refuted q[say 1; sub f(@a) {}; f(1)],
    "===SORRY!=== Error while compiling -e\nCalling f(Int) will never work with declared signature (\@a)\nat -e:1\n------> say 1; sub f(\@a) \{}; <HERE>f(1)",
    'a scalar for an @ parameter';
refuted q[say 1; sub f(Int $x where * > 0) {}; f("a")],
    "===SORRY!=== Error while compiling -e\nCalling f(Str) will never work with declared signature (Int \$x where \{ ... })\nat -e:1\n------> say 1; sub f(Int \$x where * > 0) \{}; <HERE>f(\"a\")",
    'the nominal type of a where-constrained parameter';
refuted q[say 1; sub f(:$x!) {}; f()],
    "===SORRY!=== Error while compiling -e\nCalling f() will never work with declared signature (:\$x!)\nat -e:1\n------> say 1; sub f(:\$x!) \{}; <HERE>f()",
    'a required named parameter with no named argument';
refuted q[say 1; sub f(Int $x) {}; f(Nil)],
    "===SORRY!=== Error while compiling -e\nCalling f(Nil) will never work with declared signature (Int \$x)\nat -e:1\n------> say 1; sub f(Int \$x) \{}; <HERE>f(Nil)",
    'Nil for a typed parameter';

refuted q[say 1; multi f(Int $x) {}; multi f(Num $x) {}; f("a")],
    "===SORRY!=== Error while compiling -e\nCalling f(Str) will never work with any of these multi signatures:\n    (Int \$x)\n    (Num \$x)\nat -e:1\n------> say 1; multi f(Int \$x) \{}; multi f(Num \$x) \{}; <HERE>f(\"a\")",
    'a multi none of whose candidates accepts the literal';
refuted q[say 1; proto f($) {*}; multi f(Int $x = 1) {}; f("a")],
    "===SORRY!=== Error while compiling -e\nCalling f(Str) will never work with any of these multi signatures:\n    (Int \$x = 1)\nat -e:1\n------> say 1; proto f(\$) \{*}; multi f(Int \$x = 1) \{}; <HERE>f(\"a\")",
    'a multi candidate with a default is still judged';

refuted q[say 1; sub f(Int $x) {}; f(Str)],
    "===SORRY!=== Error while compiling -e\nCalling f(Str) will never work with declared signature (Int \$x)\nat -e:1\n------> say 1; sub f(Int \$x) \{}; <HERE>f(Str)",
    'a type object unrelated to the parameter type';

# Left to the run-time binder, as in rakudo: the unit runs first.
for q[sub f(Int:D $x) {}; f("a")],
    q[sub f(Int $x = 5) {}; f("a")],
    q[sub f(Int $x is rw) {}; f("a")],
    q[sub f(Str() $x) { print "" }; f(1)],
    q[sub f($n, :$a) {}; f(1, 2)],
    q[multi f(Int $x) {}; multi f($x) { print "" }; f("a")],
    q[sub f(Int $x) {}; f(<a>)],
    q[sub f(Int $x) {}; f(qw/a/)],
    q[sub f(Int $x) {}; f(Cool)],
    q[sub f(Int $x, Str $y) {}; f(Cool, 1)],
    q[multi min(Int $a) { "u" }; my $m = min(3, 4)]
    -> $tail {
    my ($out, $err, $exit) = run-code("say 1; $tail");
    is $out, "1\n", "runs before the binder decides: $tail";
}

my ($out, $err, $exit) = run-code(q[sub f(Cool $x) {}; f(1); say "ok"]);
is $out, "ok\n", 'a call that binds is not refuted';
