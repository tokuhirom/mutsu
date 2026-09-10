use Test;

# ADR-0035 slice 2: method calls never pushed a caller-env frame, so
# `CALLER::<$*x>` read Nil instead of the caller's dynamic value, and
# `callframe(N)` inside a method body reported the method's own frame
# instead of the caller's, no matter what N was. `CompiledCode::uses_callframe`
# was already computed correctly for method bodies by the shared compiler
# (`CALLER::<$*y>` compiles to `GetCallerVar`, `callframe(N)` to
# `CallFuncNamed`) -- the bug was that neither `call_compiled_method` nor
# `call_compiled_method_fast` ever consulted it. Fixed by pushing a
# caller-env frame at both chokepoints, gated on `cc.uses_callframe`.
# See docs/adr/0035-method-calls-observe-caller-frames.md.

plan 5;

# `CALLER::<$*y>` read from inside a method body must see the caller's
# dynamic value.
class CallerVarReader {
    method reader() {
        return CALLER::<$*y>;
    }
}
sub outer-caller-var() {
    my $*y = 42;
    CallerVarReader.new.reader();
}
is outer-caller-var(), 42, 'CALLER::<$*y> read from a method sees the caller frame';

# `callframe(1).line` from inside a method must report the CALL SITE's line,
# not the method's own line.
class CallSiteLineReader {
    method reader() {
        return callframe(1).line;
    }
}
sub outer-call-site-line() {
    return CallSiteLineReader.new.reader();   # <- line 37, the call site
}
is outer-call-site-line(), 37,
    'callframe(1).line from a method reports the call-site line';

# Depth-0/depth-1 sanity: callframe(0) must still describe the method's OWN
# frame correctly -- the caller-env push must not disturb the existing
# same-frame introspection.
class DepthSanityReader {
    method reader() {
        my $own-line = callframe(0).line;   # <- line 47
        my $caller-line = callframe(1).line;
        return ($own-line, $caller-line);
    }
}
sub outer-depth-sanity() {
    return DepthSanityReader.new.reader();   # <- line 53, the call site
}
my ($own, $caller) = outer-depth-sanity();
is $own, 47, 'callframe(0).line inside a method still describes its own frame';
is $caller, 53, 'callframe(1).line from the same method reaches the call site';

# The fast dispatch path (no `is rw`/aliasable-container params, all-positional
# args) must observe the caller too -- `CALLER::` is not gated to the slow path.
class FastPathCallerVarReader {
    method reader($n) {
        return CALLER::<$*z> + $n;
    }
}
sub outer-fast-path() {
    my $*z = 100;
    FastPathCallerVarReader.new.reader(5);
}
is outer-fast-path(), 105,
    'CALLER::<$*z> read from a fast-dispatch-eligible method sees the caller frame';
