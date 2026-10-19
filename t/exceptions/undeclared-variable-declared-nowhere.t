use Test;

# A variable the unit declares nowhere is a compile-time `X::Undeclared` in
# rakudo (#12800): `$xx = 1` with no `my $xx` stops the program before
# anything runs. mutsu let the program run and failed, or silently made the
# variable up, only when the statement executed. A compile-time error cannot
# be caught in-process, so each program runs in a child process.

plan 15;

sub run-snippet(Str:D $code) {
    my $proc = run $*EXECUTABLE.absolute, '-e', $code, :out, :err;
    ($proc.exitcode, $proc.out.slurp(:close), $proc.err.slurp(:close));
}

{
    my ($status, $out, $err) = run-snippet 'say "ran"; my $x = 1; $xx = 2';
    isnt $status, 0, 'a typo of a declared variable fails the program';
    is $out, '', '...before anything runs';
    like $err, /"Variable '\$xx' is not declared"/, '...naming the variable';
}

{
    my ($status, $out, $err) = run-snippet 'use Test; say "ran"; ok $nope';
    isnt $status, 0, 'a `use Test` program is checked too';
    like $err, /"Variable '\$nope' is not declared"/, '...with the same message';
}

{
    my ($status, $out, $err) = run-snippet 'sub f { @nowhere.push(1) }; say "ran"';
    isnt $status, 0, 'a routine body that names an undeclared array fails the program';
    like $err, /"Variable '\@nowhere' is not declared"/, '...naming the array';
}

# What stays legal: every form below declares the name somewhere the scan
# has to know about, so none of them is an undeclared variable.
my @legal =
    'class C { { my $n = 0; method inc { ++$n } } }; print C.inc',
    'my $hist; BEGIN { $hist ~= "b" }; print $hist',
    'given (1, 2, 3) -> ($first, |rest) { print rest.elems }',
    'class A { has ($a, $b); method s { $a // $b // "u" } }; print A.new.s',
    'class C {}; try { C .= new }; print C.^name',
    'constant term:<$box> = 5; print $box',
    'my $self = 3; print $self',
    'print (-> $self { $self + 1 })(1)';

for @legal -> $code {
    my ($status, $out, $err) = run-snippet $code;
    unlike $err, /'is not declared'/, "legal: $code";
}
