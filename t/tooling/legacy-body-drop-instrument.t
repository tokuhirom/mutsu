use Test;

# ADR-0019 C6e-3b: a safe-class plan-derived routine registers with an EMPTY
# AST body by default and must behave identically through its attached
# bytecode. These are the representative shapes the C6e-3a drop simulation
# broke before its hardening (see news/2026-08/legacy-body-drop-groundwork.md).
# Each runs in a subprocess so the shapes exercise a fresh registration.

plan 6;

sub drop-run(Str $code) {
    my $p = run($*EXECUTABLE, "-e", $code, :out, :err);
    my $out = $p.out.slurp(:close).trim;
    my $err = $p.err.slurp(:close);
    diag $err if $err.chars && $p.exitcode != 0;
    $out
}

# A sibling-scope same-signature redefinition must install the new body —
# pre-C6e-3a the drop's empty AST body made registration mistake it for a
# forward-declaration no-op (the fix reads the plan's `body_is_empty` fact).
is drop-run(q:to/END/), "4199", 'sibling-scope redefinition installs the new body';
    { sub scoped {41}; print scoped(); }
    { sub scoped {99}; print scoped(); }
    print "\n";
    END

is drop-run(q:to/END/), "1,4,9|2,4", 'code objects drive map/grep through bytecode';
    sub sq($x) { $x * $x }
    sub even($x) { $x %% 2 }
    say (1,2,3).map(&sq).join(",") ~ "|" ~ (1,2,3,4).grep(&even).join(",");
    END

is drop-run(q:to/END/), "89", 'a wrapping trait_mod receives a runnable routine';
    our %cache;
    multi sub trait_mod:<is>(Routine $r, :$Cached!) {
        $r.wrap(-> $arg {
            %cache{$arg}:exists ?? %cache{$arg} !! (%cache{$arg} = callwith($arg))
        });
    }
    sub cfib($x) is Cached { $x <= 1 ?? 1 !! cfib($x - 1) + cfib($x - 2) }
    say cfib(10);
    END

is drop-run(q:to/END/), "escaped-ok", 'block-lexical sub survives its scope';
    my $c;
    { my sub f() { "escaped-ok" }; $c = -> { f() }; }
    say $c();
    END

ok drop-run(q:to/END/).contains("ok 1 - boom dies"), 'dies-ok runs a routine code object';
    use Test;
    plan 1;
    sub boom() { die "kaboom" }
    dies-ok &boom, 'boom dies';
    END

is drop-run(q:to/END/), "2,3,5", 'a routine Code endpoint stops the sequence';
    sub over($n) { $n > 6 }
    say (2, 3, 5, 7, 11 ... &over).head(3).join(",");
    END
