use Test;

plan 14;

# --- callwith (multi) ---
{
    multi sub cw-multi(Int $x) { callwith($x + 100) }
    multi sub cw-multi(Any $x) { "Any:$x" }
    is cw-multi(1), "Any:101", "callwith passes new args in multi dispatch";
}

# --- callwith (method) ---
{
    class CWA { method greet($x) { "A:$x" } }
    class CWB is CWA { method greet($x) { callwith($x + 100) } }
    is CWB.new.greet(1), "A:101", "callwith passes new args in method dispatch";
}

# --- callwith returns result ---
{
    multi sub cw-ret(Int $x) { my $r = callwith($x + 100); "Int:$r" }
    multi sub cw-ret(Any $x) { "Any:$x" }
    is cw-ret(1), "Int:Any:101", "callwith returns result to caller";
}

# --- nextwith (multi) ---
{
    multi sub nw-multi(Int $x) { nextwith($x + 100); "NEVER" }
    multi sub nw-multi(Any $x) { "Any:$x" }
    is nw-multi(1), "Any:101", "nextwith passes new args and does not return";
}

# --- nextwith (method) ---
{
    class NWA { method greet($x) { "A:$x" } }
    class NWB is NWA { method greet($x) { nextwith($x + 100); "NEVER" } }
    is NWB.new.greet(1), "A:101", "nextwith passes new args in method dispatch";
}

# --- nextsame (multi) ---
{
    multi sub ns-multi(Int $x) { nextsame; "NEVER" }
    multi sub ns-multi(Any $x) { "Any:$x" }
    is ns-multi(1), "Any:1", "nextsame uses original args and does not return";
}

# --- nextsame (method) ---
{
    class NSA { method greet($x) { "A:$x" } }
    class NSB is NSA { method greet($x) { nextsame; "NEVER" } }
    is NSB.new.greet(1), "A:1", "nextsame uses original args in method dispatch";
}

# --- callsame still works ---
{
    multi sub cs-multi(Int $x) { my $r = callsame; "Int:$r" }
    multi sub cs-multi(Any $x) { "Any:$x" }
    is cs-multi(1), "Int:Any:1", "callsame still works and returns result";
}

# --- X::NoDispatcher ---
{
    sub no-dispatch() { callsame }
    dies-ok { no-dispatch() }, "callsame outside dispatch throws";
}
{
    sub no-dispatch() { nextsame }
    dies-ok { no-dispatch() }, "nextsame outside dispatch throws";
}
{
    sub no-dispatch() { callwith(1) }
    dies-ok { no-dispatch() }, "callwith outside dispatch throws";
}
{
    sub no-dispatch() { nextwith(1) }
    dies-ok { no-dispatch() }, "nextwith outside dispatch throws";
}

# --- X::NoDispatcher message ---
{
    sub no-dispatch() { callsame }
    try { no-dispatch(); CATCH { default { pass "X::NoDispatcher caught" } } }
}
{
    sub no-dispatch() { callwith(42) }
    try { no-dispatch(); CATCH { default { pass "X::NoDispatcher caught for callwith" } } }
}
