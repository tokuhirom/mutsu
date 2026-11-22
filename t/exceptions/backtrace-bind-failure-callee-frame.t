use Test;

# A run-time binding failure of a sub call is reported inside the sub, as
# rakudo reports it: the backtrace starts `in sub f at ... line <declaration>`.
# That holds for a call whose argument types are all known statically too,
# when the failure is not the compile-time refutation ("will never work"): a
# `where` constraint, or a missing required named argument.

plan 4;

sub stderr-of(Str $code) {
    my $file = $*TMPDIR.add("bind-frame-{$*PID}-{(^1_000_000).pick}.raku");
    $file.spurt($code);
    my $p = run $*EXECUTABLE, $file, :err;
    my $err = $p.err.slurp(:close);
    $file.unlink;
    $err
}

my $where = stderr-of("sub g(\$x where * > 5) \{\n}\ng(3);\n");
like $where, /'Constraint type check failed'/, 'a where constraint fails';
like $where, /^^ '  in sub g at ' \N* ' line 1' $$/, '...inside the sub';

my $named = stderr-of("sub h(:\$x!) \{\n}\nmy %n;\nh(|%n);\n");
like $named, /"Required named parameter 'x' not passed"/, 'a missing required named argument';
like $named, /^^ '  in sub h at ' \N* ' line 1' $$/, '...inside the sub';
