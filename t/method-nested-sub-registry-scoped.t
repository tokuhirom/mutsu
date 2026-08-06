use Test;

# A `sub` declared directly inside a method body is lexically scoped to that
# method, same as a `sub` nested inside an ordinary `sub`/closure body — but
# method dispatch (`call_compiled_method` / `call_compiled_method_fast`,
# src/vm/vm_method_dispatch.rs) never snapshotted/restored the routine
# registry around the body the way the sub/closure call paths do
# (`call_compiled_function_fast` / `call_compiled_function_named_inner`), so
# the nested sub stayed registered and globally callable after the method
# returned. (todo/tickets/nested-sub-in-method-leaks-to-global-scope.md)

plan 7;

# --- the light/fast method-dispatch path (no params) ---
{
    class ScopedFast {
        method secret-holder() {
            sub secret() { 42 }
            secret();
        }
    }
    is ScopedFast.new.secret-holder(), 42, 'fast path: nested sub runs inside its method';
    my $leaked = try { secret() };
    nok $leaked.defined, 'fast path: nested sub does not leak out of the method';
}

# --- the general/slow method-dispatch path (a named param forces it) ---
{
    class ScopedSlow {
        method secret-holder(:$n = 5) {
            sub secret() { 42 + $n }
            secret();
        }
    }
    is ScopedSlow.new.secret-holder(), 47, 'slow path: nested sub runs inside its method';
    my $leaked = try { secret() };
    nok $leaked.defined, 'slow path: nested sub does not leak out of the method';
}

# --- a returned closure over the nested sub still escapes and works ---
{
    class Adder {
        method make-adder($base) {
            sub adder($x) { $base + $x }
            return &adder;
        }
    }
    my &f = Adder.new.make-adder(10);
    is f(5), 15, 'a nested sub returned as a closure still works after the method returns';
}

# --- two distinct instances/calls do not cross-leak each other's nested sub ---
{
    class PerCall {
        method holder($n) {
            sub secret() { $n }
            secret();
        }
    }
    my $pc = PerCall.new;
    is $pc.holder(1), 1, 'first call captures its own value';
    is $pc.holder(2), 2, 'second call captures its own (re-registered) value';
}
