use Test;

# An uncaught error from an unhandled Failure prints the fail site's
# backtrace, then "Actually thrown at:" and the site where using the Failure
# threw -- but only when the first does not already end with the second
# (rakudo's top-level handler: `!$e.gist.ends-with($v.Str)`). A Failure sunk
# in the routine that made it, or in one of its callers, shows one backtrace.

plan 6;

sub stderr-of(Str $code) {
    my $p = run $*EXECUTABLE, '-e', $code, :out, :err;
    $p.out.slurp(:close);
    $p.err.slurp(:close).trim-trailing
}

is stderr-of(q[{ fail "f" }; say "after"]),
    "f\n  in block <unit> at -e line 1",
    'sunk in the block that failed: one backtrace';
is stderr-of(q[sub ff { fail "f" }; ff(); say "after"]),
    "f\n  in sub ff at -e line 1\n  in block <unit> at -e line 1",
    'sunk in the caller: one backtrace';
is stderr-of(q[sub ff { fail "f" }; my $x = ff(); say $x + 1]),
    "f\n  in sub ff at -e line 1\n  in block <unit> at -e line 1",
    'used in the caller: one backtrace';
is stderr-of(q[sub ff { fail "f" }; sub g { my $x = ff(); $x.sink }; g()]),
    "f\n  in sub ff at -e line 1\n  in sub g at -e line 1\n  in block <unit> at -e line 1",
    'sunk two levels up: one backtrace';
is stderr-of(q[sub ff { fail "f" }; my $x = ff(); sub g { $x.sink }; g()]),
    "f\n  in sub ff at -e line 1\n  in block <unit> at -e line 1\n\nActually thrown at:\n  in sub g at -e line 1\n  in block <unit> at -e line 1",
    'sunk in another routine: both backtraces';
is stderr-of(q[sub ff { fail "f" }; sub g { my $x = ff(); sub hh { $x.sink }; hh() }; g()]),
    "f\n  in sub ff at -e line 1\n  in sub g at -e line 1\n  in block <unit> at -e line 1\n\nActually thrown at:\n  in sub hh at -e line 1\n  in sub g at -e line 1\n  in block <unit> at -e line 1",
    'sunk in a routine the caller called: both backtraces';
