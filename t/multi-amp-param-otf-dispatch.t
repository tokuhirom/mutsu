use Test;

# Multi candidates with a `&callback` parameter are now OTF-compiled (run as
# compiled bytecode) instead of forced through the interpreter fallback (ledger
# §D, multi-dispatch VM-ization). `def_is_otf_compilable` previously excluded any
# param whose name starts with `&`; that exclusion is removed (params with a
# *code signature* `&cb:(...)` or a default value remain excluded). This is a
# fallback-elimination change: behavior is byte-identical, dispatch just no
# longer bounces through the tree-walk interpreter.

plan 8;

# bare multi with a callback candidate (block literal is Callable)
{
    multi bb(&cb) { cb() + 1 }
    multi bb(Int $x) { $x }
    is bb({ 41 }), 42, 'bare multi &cb candidate runs the block';
    is bb(7), 7, 'bare multi Int candidate still selected';
}

# callback called with arguments
{
    multi nn(&fn, Int $n) { fn($n) * 2 }
    multi nn(Int $n) { $n }
    is nn(-> $x { $x + 100 }, 5), 210, 'callback invoked with an argument';
    is nn(9), 9, 'non-callback candidate still selected';
}

# a named sub passed by &name
{
    sub helper($x) { $x * 3 }
    multi pp(&f) { f(4) }
    is pp(&helper), 12, 'named sub passed by &name and invoked';
}

# callback closing over an outer variable
{
    my $base = 1000;
    multi cc(&g) { g() + $base }
    is cc({ 5 }), 1005, 'callback closes over outer lexical';
}

# proto-dispatched callback candidate
{
    proto pr(|) {*}
    multi pr(&cb) { "cb:" ~ cb() }
    multi pr(Int $x) { "int:$x" }
    is pr({ 41 }), 'cb:41', 'proto-dispatched &cb candidate';
    is pr(7), 'int:7', 'proto-dispatched Int candidate';
}
