# `callsame()` skips the generic call funnel

An argument-less `callsame()` / `nextsame()` used to walk the whole generic
function-call funnel before it reached the builtin: the compiled-function and
multi-candidate lookups, `resolve_function_with_types`, the native tables and
the lexical `&name` probes. Each of them answered "not a user routine" first.
A new lane (`src/vm/vm_call_dispatcher_builtin.rs`) checks once that nothing
shadows the builtin. That covers a lexical or env `&callsame`, a user routine,
multi or proto of the name, a wrap, or an EXPORT-hook override. When none
exists, it runs the same carrier terminal directly. The lane also accepts the
call-site line marker that the parser stamps on every empty `name()` call,
which is the reason a plain `callsame()` never qualified for a narrower
argument-less check.

A routine return also stopped asking `is_var_dynamic` about every key of the
caller's env once any dynamic variable exists. Only a `*`-twigil key, or a
flagged name without the twigil, can be dynamic, and the symbol's memoized
flags answer the twigil question without a string lookup.

On #10110's FiniteField-style repro (`multi infix:<*>(UInt $a, UInt $b -->
UInt) { callsame() mod $*modulus }` over 15,000 iterations), callgrind went
from 1.847G to 1.418G instructions (-23%), on a profiling build of the same
base.
