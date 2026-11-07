use v6;
use Test;

# `exit` run inside a CATCH handler ends the program with that status; the
# exception that was unwinding is neither reported nor does it change the
# status (found via Slang::Date's t/basic.t: `CATCH { is ...; exit }`).

plan 4;

sub run-code(Str $code) {
    my $proc = run $*EXECUTABLE, '-e', $code, :out, :err;
    my $out = $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);
    ($proc.exitcode, $out, $err)
}

my ($rc, $out, $err) = run-code("\{ die 'boom'; CATCH \{ say 'in catch'; exit } }\nsay 'after'");
is $rc, 0, 'status 0 after exit in a handler without when/default';
is $out, "in catch\n", 'nothing runs after the exit';
is $err, '', 'the in-flight exception is not reported';

($rc, $out, $err) = run-code("\{ die 'boom'; CATCH \{ exit 3 } }\nsay 'after'");
is $rc, 3, 'the exit status of the handler wins';
