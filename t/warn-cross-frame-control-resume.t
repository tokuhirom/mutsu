use Test;

# A `warn` raised deep inside a sub/method call chain, caught by a unit-level
# `CONTROL { default { ...; .resume } }` handler, must run that handler at the
# raise site and *resume* the deep computation — the code after the `warn`
# (several Rust frames down) still runs and its value flows back out. Previously
# mutsu unwound the Rust call stack to the CONTROL frame, destroying every frame
# between the warn and the handler, so the rest of the deep computation was lost
# (the Template::Mustache 06-logging blocker). The handler is "resume-safe" (it
# unconditionally `.resume`s with no `when`/`succeed` exit), so it is run inline
# at the raise site rather than via the unwinding path.

plan 7;

# --- canonical deep cross-frame resume ---
{
    my $out = '';
    sub deep { warn "deep warn"; "after-warn" }
    sub render { my $r = deep(); $out ~= "[render-continued:$r]"; }
    CONTROL { default { $out ~= "<caught:{.message.lines[0]}>"; .resume; } }
    render();
    is $out, '<caught:deep warn>[render-continued:after-warn]',
        'deep warn caught by unit CONTROL{default{.resume}} resumes; deep frame continues';
}

# --- handler sees the CX::Warn message ---
{
    my @seen;
    sub w-deep { warn "w1"; warn "w2"; "done" }
    CONTROL { default { @seen.push(.message.lines[0]); .resume } }
    my $r = w-deep();
    is $r, 'done', 'multiple deep warns all resume; routine returns its value';
    is @seen, ['w1', 'w2'], 'CONTROL handler observed each warn message in order';
}

# --- warn through a method chain ---
{
    my $log = '';
    class C {
        method deep() { warn "method warn"; 42 }
        method top()  { self.deep() + 1 }
    }
    CONTROL { default { $log ~= .message.lines[0]; .resume } }
    is C.new.top(), 43, 'warn deep in a method chain resumes; value flows out';
    is $log, 'method warn', 'method-chain warn reached the CONTROL handler';
}

# --- indirect &warn.() deep in the chain (the Template::Mustache logger shape) ---
{
    my $caught = '';
    sub inner { my $w = &warn; $w.("indirect deep"); "inner-ok" }
    sub mid   { "[" ~ inner() ~ "]" }
    CONTROL { default { $caught = .message.lines[0]; .resume } }
    is mid(), '[inner-ok]', 'indirect &warn.() deep resumes; caller value preserved';
}

# --- a `when CX::Warn { }` handler still SUCCEEDS (exits), it does NOT resume ---
# This guards the #3372 killer: a non-resume-safe handler must take the unwinding
# path so the implicit `succeed` has a block boundary to exit to.
{
    my $after = 'not-run';
    my $r = do {
        warn "exit me";
        $after = 'ran';
        'body-value';
        CONTROL { when CX::Warn { 'handled' } }
    };
    is $after, 'not-run',
        'when CX::Warn succeeds (exits the block); code after the warn does NOT run';
}
