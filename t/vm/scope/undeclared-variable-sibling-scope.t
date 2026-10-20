use Test;

# A variable only a scope that does not enclose its use declares is a
# compile-time `X::Undeclared` in rakudo (#12800): `sub f { $x }; { my $x }`
# stops the program before anything runs. mutsu left such a name to run
# time, where a routine resolved it through its caller's env. A compile-time
# error cannot be caught in-process, so each program runs in a child
# process.

plan 23;

sub run-snippet(Str:D $code) {
    my $proc = run $*EXECUTABLE.absolute, '-e', $code, :out, :err;
    ($proc.exitcode, $proc.out.slurp(:close), $proc.err.slurp(:close));
}

for 'say "ran"; sub f { $x = 1 }; { my $x = 2 }',
    'say "ran"; { my $x = 1 }; say $x',
    'say "ran"; for 1 -> $i { }; say $i',
    'say "ran"; class C { method m { my $q } }; sub g { $q }'
-> $code {
    my ($status, $out, $err) = run-snippet $code;
    isnt $status, 0, "rejected: $code";
    is $out, '', '...before anything runs';
    like $err, /"is not declared"/, '...as an undeclared variable';
}

# What stays legal: each form declares the name in a scope the use sees,
# though the lowered AST moves the declaration or spells it as no `my`.
my @legal =
    'try my $value = 1; print $value',
    'once my $o = 1; print $o',
    'for my @a = 1, 2 { }; print @a',
    'given my $g = 2 { }; print $g',
    '"aba" ~~ / (a) b :my $c = ~$0; /; print $c',
    'given (1, 2) -> ($a, |rest) { print rest.elems }',
    'my $n = 1; given $n -> (|c) { print c.elems }',
    'class R { { method m($class: ) { $class } } }; print R.m.^name',
    'class R { { my $q = 1 }; method m { 1 } }; print R.m',
    'my $s = supply { my $stop = False; emit 1; CLOSE { $stop = True } }; $s.tap(&print)',
    'class K { { my $q = 1 }; { my $r = 2 } }; print 1';

for @legal -> $code {
    my ($status, $out, $err) = run-snippet $code;
    unlike $err, /'is not declared'/, "legal: $code";
}
