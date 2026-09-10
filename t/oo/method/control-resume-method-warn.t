use Test;

# A resumable `warn` raised by a method call must be resumable by an enclosing
# `CONTROL { .resume }`, regardless of whether the receiver is a literal (the
# `CallMethod` opcode), a mutable variable (`CallMethodMut`), or each element of
# a hyper call (`@a>>.meth`). Regression: only the literal-receiver path
# recorded a resume point, so `$s.indent(-N)` and `@a>>.indent(-N)` under a
# CONTROL block abandoned the rest of the enclosing block (roast S32-str/indent.t).

plan 5;

# Variable receiver: CallMethodMut path.
{
    my @log;
    my $s = "A";
    {
        CONTROL { default { .resume } }
        my $r = $s.indent(-4);
        @log.push: "resumed:$r";
    }
    is @log, ["resumed:A"], 'CONTROL resumes a warn from a var-receiver method';
}

# Literal receiver: CallMethod path (was already working -- guard against regress).
{
    my @log;
    {
        CONTROL { default { .resume } }
        my $r = "A".indent(-4);
        @log.push: "resumed:$r";
    }
    is @log, ["resumed:A"], 'CONTROL resumes a warn from a literal-receiver method';
}

# Hyper receiver: each element warns; CONTROL resumes the whole expression.
{
    my @log;
    my @c = "A", "B", "C";
    {
        CONTROL { default { .resume } }
        my @r = @c>>.indent(-4);
        @log.push: "elems:" ~ @r.elems;
        @log.push: "join:" ~ @r.join(",");
    }
    is @log, ["elems:3", "join:A,B,C"],
        'CONTROL resumes the whole hyper result through per-element warns';
}

# The block after a resumed hyper warn keeps executing (no early abort).
{
    my $reached = False;
    my @c = "X";
    {
        CONTROL { default { .resume } }
        my @r = @c>>.indent(-9);
        $reached = True;
    }
    ok $reached, 'execution continues past a resumed hyper warn';
}

# A non-warning hyper method is unaffected.
{
    my @c = "a", "b";
    is-deeply @c>>.uc, ["A", "B"], 'normal hyper method still works';
}
