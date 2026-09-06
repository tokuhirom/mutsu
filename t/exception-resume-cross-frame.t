use v6;
use Test;

# ADR-0072: a resumable exception runs its CATCH handler at the throw point.
#
# Every row of the measured raku-vs-mutsu table in the ADR is pinned here,
# INCLUDING the rows that already passed before the change (a resumption change
# is most likely to break exactly those). Each expectation was taken from `raku`.

plan 21;

class MyResumeEx is Exception { method message { "custom" } }
sub MyResumeEx-caller { MyResumeEx.new.throw; return "B" }

# --- Rows that already worked: the regression guard ------------------------

# 01/14: `.resume` on a `warn` (a CONTROL exception), the case the pre-existing
# inline mechanism was built for -- at one and at two sub-frames of depth. Note
# that a `warn` is seen by CONTROL, not by CATCH: with only a CATCH block the
# default warn handler prints to stderr and resumes on its own (pinned first).
{
    my $log = '';
    sub w-one { warn "w"; $log ~= "after;"; "ret" }
    my $r;
    {
        $r = w-one();
        CATCH { default { $log ~= "caught;"; .resume } }
    }
    is $log, 'after;', 'a CATCH does not see a warn; the default handler resumes it';
    is $r, 'ret', 'warn with only a CATCH present: the sub completes normally';
}

{
    my $log = '';
    sub w-deep { warn "w"; $log ~= "w-deep-after;"; "W" }
    sub w-mid { my $a = w-deep(); $log ~= "w-mid-after($a);"; "W2" }
    my $r;
    {
        $r = w-mid();
        CONTROL { default { $log ~= "caught;"; .resume } }
    }
    is $log, 'caught;w-deep-after;w-mid-after(W);',
        'a resuming CONTROL handler two frames up resumes through both frames';
    is $r, 'W2', 'warn two frames up: the outer sub completes normally';
}

# 02: die and CATCH in the same bare block.
{
    my $log = '';
    {
        $log ~= "before;";
        die "boom";
        $log ~= "after;";
        CATCH { default { $log ~= "caught({.Str});"; .resume } }
    }
    is $log, 'before;caught(boom);after;', 'die + CATCH in the same block resumes';
}

# 17: die and CATCH in the same *sub* body.
{
    my $log = '';
    sub same-sub {
        die "same";
        $log ~= "after;";
        return "B";
        CATCH { default { $log ~= "caught;"; .resume } }
    }
    is same-sub(), 'B', 'die + CATCH in the same sub body resumes and returns';
    is $log, 'caught;after;', 'die + CATCH in the same sub body: statements after the die run';
}

# 15: a CATCH that does NOT resume abandons the block.
{
    my $log = '';
    sub nr-bad { die "nr"; $log ~= "never;"; "B" }
    {
        my $r = nr-bad();
        $log ~= "unreached($r);";
        CATCH { default { $log ~= "caught;" } }
    }
    is $log, 'caught;', 'a CATCH without .resume still abandons the protected block';
}

# 11: `fail` produces a Failure and fires no CATCH.
{
    my $log = '';
    sub f-bad { fail "f-boom"; $log ~= "never;"; "B" }
    {
        my $r = f-bad();
        $log ~= "got-failure({$r ~~ Failure ?? 'yes' !! 'no'});";
        CATCH { default { $log ~= "caught;"; .resume } }
    }
    is $log, 'got-failure(yes);', 'fail returns a Failure rather than firing the CATCH';
}

# --- The rows ADR-0072 fixes ----------------------------------------------

# 03: one sub-frame up -- the ticket's headline repro.
{
    my $out = '';
    sub bad-sub { die "Something bad happened"; return "not returning" }
    {
        my $return = bad-sub;
        $out ~= "Returned $return";
        CATCH { default { $out ~= "Error {.^name}; "; $return = '0'; .resume } }
    }
    is $out, 'Error X::AdHoc; Returned not returning',
        '.resume returns to the die call site inside a nested sub';
}

# 04/05: two and three sub-frames up.
{
    my $log = '';
    sub d-in { die "deep"; $log ~= "in;"; "I" }
    sub d-out { my $x = d-in(); $log ~= "out($x);"; "O" }
    my $r;
    {
        $r = d-out();
        CATCH { default { $log ~= "caught;"; .resume } }
    }
    is "$log|$r", 'caught;in;out(I);|O', '.resume through two sub-frames';
}

{
    my $log = '';
    sub g1 { die "d1"; $log ~= "g1;"; 1 }
    sub g2 { my $a = g1(); $log ~= "g2($a);"; 2 }
    sub g3 { my $b = g2(); $log ~= "g3($b);"; 3 }
    my $r;
    {
        $r = g3();
        CATCH { default { $log ~= "caught;"; .resume } }
    }
    is "$log|$r", 'caught;g1;g2(1);g3(2);|3', '.resume through three sub-frames';
}

# 06: a die inside a loop within the sub resumes on every iteration.
{
    my $n = 0;
    sub looper {
        my @out;
        for 1..3 -> $i {
            die "iter $i" if $i != 2;
            @out.push($i);
        }
        return @out.join(",");
    }
    my $r;
    {
        $r = looper();
        CATCH { default { $n++; .resume } }
    }
    is "$n|$r", '2|1,2,3', '.resume inside a loop body resumes each iteration';
}

# 07: a die inside a nested if/block within the sub.
{
    my $log = '';
    sub nested {
        if True {
            {
                die "inner";
                $log ~= "still-inside;";
            }
            $log ~= "after-inner;";
        }
        return "N";
    }
    my $r;
    {
        $r = nested();
        CATCH { default { $log ~= "caught;"; .resume } }
    }
    is "$log|$r", 'caught;still-inside;after-inner;|N',
        '.resume from a die in a nested block inside a sub';
}

# 08: .resume inside a `try {}` rather than a bare block with CATCH.
{
    my $log = '';
    sub t-bad { die "t-boom"; $log ~= "bad-after;"; "B" }
    my $v = try {
        my $r = t-bad();
        $log ~= "in-try($r);";
        CATCH { default { $log ~= "caught;"; .resume } }
        "try-value"
    };
    is "$log|$v", 'caught;bad-after;in-try(B);|try-value',
        'a try block resumes and still yields its tail value';
}

# 09: the resumed die expression evaluates to Any.
{
    my $seen;
    sub v-bad { my $x = (die "v-boom"); $seen = $x; return "B" }
    my $r;
    {
        $r = v-bad();
        CATCH { default { .resume } }
    }
    ok !$seen.defined && $r eq 'B', 'a resumed die expression evaluates to an undefined value';
}

# 10: a user Exception subclass thrown with .throw.
{
    my $log = '';
    my $r;
    {
        $r = MyResumeEx-caller();
        CATCH { default { $log ~= "caught({.^name});"; .resume } }
    }
    is "$log|$r", 'caught(MyResumeEx);|B', '.resume works for a user Exception .throw';
}

# 12: calling .resume twice -- the first one exits the handler.
{
    my $log = '';
    sub tw-bad { die "twice"; $log ~= "bad-after;"; "B" }
    my $r;
    {
        $r = tw-bad();
        CATCH { default { $log ~= "caught;"; .resume; $log ~= "unreached;"; .resume } }
    }
    is "$log|$r", 'caught;bad-after;|B', 'the first .resume exits the handler';
}

# 16: the handler runs in the dynamic scope of the throw.
{
    my $seen = '';
    sub s-bad { my $*WHERE = "inside-bad"; die "scope"; "B" }
    {
        s-bad();
        CATCH { default { $seen = $*WHERE; .resume } }
    }
    is $seen, 'inside-bad', 'a resuming handler runs in the dynamic scope of the throw';
}

# 18: a handler that mutates an installing-frame lexical, then resumes.
{
    my $log = '';
    sub m-bad { die "mut"; $log ~= "bad-after;"; "B" }
    my $r;
    {
        $r = m-bad();
        CATCH { default { $log ~= "H;"; .resume } }
    }
    is "$log|$r", 'H;bad-after;|B', "a resuming handler's writes to an outer lexical survive";
}

# --- Recorded residuals (ADR-0072 Slices 2 and 3) --------------------------

# 13: an exception an inner, non-resume-capable CATCH rethrew cannot be
# inline-resumed by an outer one -- the inner marker blocks the inline path so
# the inner handler is not skipped. ADR-0072 Slice 2.
{
    my $log = '';
    sub rt-bad { die "rt"; $log ~= "bad-after;"; "B" }
    sub rt-mid {
        my $r = rt-bad();
        $log ~= "mid($r);";
        return "M";
        CATCH { default { $log ~= "inner;"; .rethrow } }
    }
    my $r = 'unset';
    {
        $r = rt-mid();
        CATCH { default { $log ~= "outer;"; .resume } }
    }
    todo 'ADR-0072 Slice 2: resuming a rethrown exception needs the whole handler chain inline';
    is "$log|$r", 'inner;outer;bad-after;mid(B);|M', '.resume of a rethrown exception';
}
