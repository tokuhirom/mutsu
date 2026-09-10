use Test;

plan 8;

# A `try` block is a normal block which implicitly turns on `use fatal` for
# its whole lexical scope — body and CATCH/CONTROL handlers alike
# (raku-doc/doc/Language/exceptions.rakudoc, "try blocks"). An unhandled
# Failure sunk anywhere inside a `try` must throw immediately instead of
# staying a soft value, even when it is not the try block's own final
# expression (a mid-body statement's Failure was previously never sunk,
# which broke code like Cro::HTTP::Session::Persistent's
# `try { my $s = self.load($id); $req.auth = $s; CATCH { ... } }` — the
# CATCH never fired for an expired/missing session).

sub loadit() { fail("nope") }

# The same call with no surrounding `try` stays a soft Failure (sanity check
# that this is `try`-specific, not a general fail()-is-now-fatal change).
{
    my $x = loadit();
    ok !$x.defined, "outside try, fail() stays a soft, undefined Failure";
}

# Inside `try`, an unhandled Failure assigned mid-body (not the try's last
# expression) throws and is caught by CATCH.
{
    my $caught = False;
    try {
        my $y = loadit();
        $caught = False; # would run if the assignment above didn't explode
        CATCH { default { $caught = True; } }
    }
    ok $caught, "try makes a mid-body fail() throw, caught by CATCH";
}

# Without an explicit CATCH, `try` still contains the exception (implicit
# CATCH), so the mid-body statement after the failing one never runs.
{
    my $ran-after = False;
    try {
        my $y = loadit();
        $ran-after = True;
    }
    ok !$ran-after, "try without explicit CATCH still stops at the fail()";
}

# A bare block with a user CATCH (no `try`) does NOT get implicit fatal —
# only genuine `try` does.
{
    my $ran-after = False;
    {
        my $y = loadit();
        $ran-after = True;
        CATCH { default { } }
    }
    ok $ran-after, "a bare block with CATCH (not try) does not imply use fatal";
}

# A declared return type on the callee does not, by itself, make fail() throw
# outside of a `try` (this is `try`'s doing, not a return-type check).
{
    sub loadit_typed(--> Int) { fail("nope int") }
    my $z = loadit_typed();
    ok !$z.defined, "declared return type alone does not force an immediate throw";
}

# fatal_mode set by `try` extends into the CATCH handler body too.
{
    my $threw-in-catch = False;
    dies-ok {
        try {
            die "boom";
            CATCH {
                default {
                    loadit(); # this must throw, escaping the CATCH itself
                    $threw-in-catch = False;
                }
            }
        }
    }, "fatal mode set by try is still active inside its own CATCH handler";
}

# The try's own final-expression Failure is still just caught into $! (not
# exploded) — only a Failure *sunk mid-body* explodes.
{
    my $r = try { loadit() };
    ok !$r.defined, "the try block's own trailing Failure result is not exploded";
    is $!.^name, 'X::AdHoc', '$! is set to the trailing Failure exception';
}
