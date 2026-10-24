use v6;
use Test;

# `-c` compiles the program, runs its BEGIN prologue and CHECK phasers, prints
# `Syntax OK` and runs nothing else: no INIT, no mainline, no END (mutsu#14929,
# the `static` distribution's `$*EXECUTABLE -c -e ...` gate).
plan 9;

sub compile-only(Str $code) {
  my $p = run $*EXECUTABLE, '-c', '-e', $code, :out, :err;
  ($p.exitcode, $p.out.slurp(:close), $p.err.slurp(:close))
}

my ($code, $out, $err) = compile-only('say "main"; BEGIN { say "begin" }; CHECK { say "check" }; INIT { say "init" }; END { say "end" }');
is $code, 0, 'exits 0';
is $out, "begin\ncheck\nSyntax OK\n", 'BEGIN and CHECK ran, then Syntax OK; nothing else';

($code, $out) = compile-only('CHECK { say "c1" }; CHECK { say "c2" }; say "main"');
is $out, "c2\nc1\nSyntax OK\n", 'CHECK phasers run last-declared first';

($code, $out) = compile-only('sub f { 1 }; say f(); die "not run"');
is $code, 0, 'a mainline that would die is never run';
is $out, "Syntax OK\n", 'and prints only Syntax OK';

($code, $out, $err) = compile-only('sub f( {');
is $code, 1, 'a syntax error exits 1';
unlike $out, /'Syntax OK'/, 'and does not claim Syntax OK';
like $err, /'SORRY'|'Confused'|'Missing'/, 'and reports the error';

($code, $out) = compile-only('BEGIN { say "only begin" }');
is $out, "only begin\nSyntax OK\n", 'a unit with just a BEGIN';
