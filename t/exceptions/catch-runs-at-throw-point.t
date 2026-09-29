use v6;
use Test;

# ADR-0072 Slices 2-3: every CATCH handler runs at the throw point, in the
# dynamic scope of the `die`, before anything unwinds -- innermost first. Each
# expectation was taken from `raku`.

plan 13;

# The issue's repro: an inner handler that rethrows passes the exception on to
# an outer one, still at the throw point, so the outer `.resume` reaches the
# original `die` even though the inner CATCH shares its sub.
{
    my @log;
    sub rt-inner { die "x"; @log.push: "after"; CATCH { default { @log.push: "inner"; .rethrow } } }
    sub rt-outer { rt-inner(); @log.push: "outer-after"; CATCH { default { @log.push: "outer"; .resume } } }
    rt-outer();
    is @log.join(","), 'inner,outer,after,outer-after', 'resuming a rethrown exception from the same sub';
}

# The handler runs before the dying sub's LEAVE, even when it does not resume.
{
    my @log;
    sub lv { LEAVE @log.push: "leave"; die "boom" }
    {
        lv();
        CATCH { default { @log.push: "handler" } }
    }
    is @log.join(","), 'handler,leave', 'a non-resuming handler runs before the LEAVE of the dying sub';
}

# ... and sees the dynamic variables of the dying sub.
{
    my $seen;
    sub dy { my $*WHERE = "inner"; die "d" }
    {
        my $*WHERE = "outer";
        dy();
        CATCH { default { $seen = $*WHERE } }
    }
    is $seen, 'inner', 'a non-resuming handler runs in the dynamic scope of the throw';
}

# A handler that matches nothing declines; the next outer one handles it, and
# neither runs twice.
{
    my @log;
    sub dc-bad { die "dc" }
    sub dc-mid { dc-bad(); CATCH { when X::Numeric::Real { @log.push: "wrong" } } }
    {
        dc-mid();
        CATCH { default { @log.push: "outer:" ~ .message } }
    }
    is @log.join(","), 'outer:dc', 'a declining inner handler passes the exception outward';
}

{
    my $runs = 0;
    sub tw-bad { die "tw" }
    sub tw-mid { tw-bad(); CATCH { default { $runs++; .rethrow } } }
    {
        tw-mid();
        CATCH { default { $runs += 10 } }
    }
    is $runs, 11, 'each handler of a rethrow chain runs exactly once';
}

# A handler that handles without resuming ends its own block, with the frames
# below it abandoned.
{
    my @log;
    sub hd-bad { die "hd"; @log.push: "not-resumed" }
    {
        hd-bad();
        @log.push: "not-reached";
        CATCH { default { @log.push: "handled" } }
    }
    @log.push: "after-block";
    is @log.join(","), 'handled,after-block', 'a handled exception abandons its block';
}

# A handler's writes to an outer lexical survive, without a resume.
{
    my $count = 0;
    sub ct-bad { die "ct" }
    for 1..3 { ct-bad(); CATCH { default { $count++ } } }
    is $count, 3, "a non-resuming handler's writes to an outer lexical survive";
}

# The handler's lexicals are its own, not the dying sub's same-named ones.
{
    my $x = 'outer';
    my $seen;
    sub sh-bad { my $x = 'inner'; die "sh" }
    { sh-bad(); CATCH { default { $seen = $x } } }
    is $seen, 'outer', 'the handler sees its own lexical, not a same-named one in the dying sub';
}

# ... also when the handler lives in a closure that reads the capture by name.
{
    sub cl-run(&blk) { blk() }
    sub cl-inner(*%matcher) { die "cl" }
    sub cl-outer($code, *%matcher) {
        my $seen;
        cl-run { CATCH { default { $seen = %matcher.keys.join(",") } }; $code() }
        $seen
    }
    is cl-outer({ cl-inner(:instead) }, message => 1), 'message',
        "a closure handler sees its own capture, not the dying routine's same-named one";
}

# A control signal from the handler is raised at the throw point: `next`
# reaches the loop innermost at the `die`.
{
    my @log;
    sub nx-bad { for 1..3 -> $i { die "i$i" if $i == 2; @log.push: "g$i" } }
    for 1..2 -> $j {
        nx-bad();
        CATCH { default { @log.push: "c$j"; next } }
    }
    is @log.join(","), 'g1,c1,g3,g1,c2,g3', 'next in a handler reaches the loop innermost at the throw';
}

# ... while `return` still returns from the routine that installed the handler.
{
    sub rn-bad { for 1..3 { die "r" } }
    sub rn { rn-bad(); "fell-through"; CATCH { default { return "returned" } } }
    is rn(), 'returned', 'return in a handler returns from the installing routine';
}

# A handler calls a routine private to the package that installed it.
{
    module CatchPkg {
        sub helper($m) { "helper:$m" }
        our sub run(&code) {
            my $r;
            { code(); CATCH { default { $r = helper(.message) } } }
            $r
        }
    }
    is CatchPkg::run({ die "m" }), 'helper:m', "a handler resolves routines in its own package";
}

# A recursive routine: the handler of the outer activation sees its own frame.
{
    my @log;
    sub rec($n) {
        die "bottom" if $n == 0;
        rec($n - 1);
        CATCH { default { @log.push: "h$n"; .rethrow if $n < 2 } }
    }
    rec(2);
    is @log.join(","), 'h0,h1,h2', 'recursive activations each run their own handler once';
}
