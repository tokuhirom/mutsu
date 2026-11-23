use v6;
use Test;

# A `BEGIN` of the program's top level that reads or writes the unit's plain
# `my` variables runs when the parser reaches it (ADR-17558 §7.17): it sees
# them without their initializers, and what it leaves in them is what the
# mainline starts from. Each case runs a child process; every expectation is
# what rakudo prints.

plan 12;

sub run-code(Str $code) {
    my $proc = run $*EXECUTABLE, '-I', 't/lib', '-e', $code, :out, :err;
    (exitcode => $proc.exitcode, out => $proc.out.slurp(:close), err => $proc.err.slurp(:close))
}

{
    my %r = run-code(q:to/CODE/);
        my $n = 0;
        BEGIN $n++;
        BEGIN say $n;
        1 +;
        CODE
    is %r<out>, "1\n", 'BEGINs over a unit variable run before a later syntax error';
    isnt %r<exitcode>, 0, 'the syntax error still fails the program';
}

{
    my %r = run-code(q:to/CODE/);
        my @a;
        BEGIN @a.push(1);
        use LoadsAtParseTime;
        BEGIN say @a;
        say "run";
        CODE
    is %r<out>, "LoadsAtParseTime loaded\n[1]\nrun\n",
        'a use after such a BEGIN still loads at parse time, in source order';
}

my @cases =
    'my @a; BEGIN @a.push(1); say @a;'                           => "[1]\n",
    'my $n = 5; BEGIN say $n.raku; say $n;'                      => "Any\n5\n",
    'my @log; BEGIN @log.push("b"); @log.push("r"); say @log;'   => "[b r]\n",
    'my $x = 1; BEGIN $x = 3; say $x;'                           => "1\n",
    'my %h; BEGIN %h<a> = 1; say %h;'                            => "\{a => 1}\n",
    'my @a; BEGIN { @a.push($_) for ^3 }; say @a;'               => "[0 1 2]\n",
    'my $x; BEGIN $x = [1,2]; say $x.raku;'                      => "\$[1, 2]\n",
    'my @a = 1,2; BEGIN @a.push(9); say @a;'                     => "[1 2]\n",
    'my @a; BEGIN @a.push(1); BEGIN @a.push(2); say @a;'         => "[1 2]\n";

for @cases -> (:key($code), :value($expected)) {
    is %(run-code($code))<out>, $expected, $code;
}
