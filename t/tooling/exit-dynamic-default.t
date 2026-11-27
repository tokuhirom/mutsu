use v6;
use Test;

# Rakudo binds the process-level `&*EXIT` to the `exit` routine itself, so a
# program (zef's `MAIN` `CATCH`) can end with `&*EXIT(1)` (#19247).
plan 5;

is &*EXIT.name, 'exit', '&*EXIT is the exit routine';
ok &*EXIT ~~ Callable, '&*EXIT is callable';

sub run-code(Str $code) {
    my $proc = run $*EXECUTABLE.absolute, '-e', $code, :out, :err;
    my $out = $proc.out.slurp(:close);
    $proc.err.slurp(:close);
    ($proc.exitcode, $out)
}

is-deeply run-code('&*EXIT(3); print "not reached"'), (3, ''),
    'calling &*EXIT exits with its status';
is-deeply run-code('CATCH { default { &*EXIT(1) } }; die "boom"'), (1, ''),
    'the zef CATCH idiom exits with 1';
is-deeply run-code('sub f { my &*EXIT = -> $c { print "trapped $c" }; exit 7 }; f()'),
    (0, 'trapped 7'),
    'a caller-installed &*EXIT still intercepts exit';
