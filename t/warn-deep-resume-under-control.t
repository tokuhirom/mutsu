use Test;

# A resumable `warn` raised *deep* inside a chain of nested calls must let an
# enclosing `CONTROL {}` `.resume` so the deep call CONTINUES. Previously the
# warn unwound to the installing block, whose frame-relative resume point pointed
# into the wrong code, crashing with "No such method 'CALL-ME'..." or silently
# truncating. The handler now runs inline in the warn's still-live frame.
# (Template::Mustache logs warnings this way: render -> ... -> &warn.().)

plan 6;

# --- warn one level deep, resumed; caller continues to a 2nd warn ---
{
    my @seen;
    CONTROL { default { @seen.push(.message); .resume; } }
    sub emit { warn "a"; warn "b"; "done" }
    my $r = emit();
    is @seen.join(','), 'a,b', 'both warns from a called sub are caught';
    is $r, 'done', 'the called sub runs to completion after resuming';
}

# --- warn two levels deep ---
{
    my @seen;
    CONTROL { default { @seen.push(.message); .resume; } }
    sub inner { warn "deep"; 42 }
    sub outer { my $x = inner(); $x + 1 }
    my $r = outer();
    is @seen.join(','), 'deep', 'a warn two calls deep is caught';
    is $r, 43, 'both frames continue after the deep warn resumes';
}

# --- warn via an indirect &warn.() deep in a sub ---
{
    my @seen;
    CONTROL { default { @seen.push(.message); .resume; } }
    sub logit($w, $msg) { $w.($msg) }
    my $w = &warn;
    logit($w, "x");
    logit($w, "y");
    is @seen.join(','), 'x,y', 'indirect &warn.() deep in a sub resumes';
}

# --- a deep warn with NO CONTROL just prints and the call still completes ---
{
    sub noisy { warn "ignored"; "ok" }
    # No CONTROL here: the warn prints to stderr (not captured) but must not crash.
    is noisy(), 'ok', 'an uncaught deep warn does not abort the calling sub';
}
