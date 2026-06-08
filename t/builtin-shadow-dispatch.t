use Test;

# Regression pin for ③ PR-2: a user-defined sub/multi that shadows a same-named
# builtin is now run as compiled bytecode (OTF compile) when its def is simple
# enough, instead of tree-walking through call_function_fallback. The dispatch
# must still NOT fall through to native (which would pick the shadowed builtin),
# and complex-bodied / complex-signature shadows keep working via the fallback.

plan 9;

# Simple shadow of a 1-arg builtin (abs) -> user version wins, runs compiled.
sub abs($x) { "myabs:$x" } #OK shadow
is abs(-5), "myabs:-5", 'simple shadow of abs runs the user sub';

# Shadow of an aggregate builtin (elems).
sub elems(@a) { 42 } #OK shadow
is elems([1, 2, 3, 4]), 42, 'shadow of elems runs the user sub';

# Shadow invoked through a Slip argument (exec_call_func_slip_op path).
sub join(*@a) { "J(" ~ @a.elems ~ ")" } #OK shadow
my @x = (1, 2, 3);
is join(|@x), "J(3)", 'shadow dispatched via slip argument';

# Multi sub shadowing a builtin (min) dispatches by arg type.
multi sub min(Int $a) { "int:$a" } #OK shadow
multi sub min(Str $a) { "str:$a" } #OK shadow
is min(3), "int:3", 'multi shadow dispatches Int candidate';
is min("hi"), "str:hi", 'multi shadow dispatches Str candidate';

# Default-param shadow is NOT OTF-compilable -> keeps working via the fallback.
sub chars($s = "def") { "C:$s" } #OK shadow
is chars(), "C:def", 'default-param shadow (fallback path) uses default';
is chars("xy"), "C:xy", 'default-param shadow (fallback path) with arg';

# Shadow body that itself calls another builtin still resolves correctly.
sub sum(@a) { [+] @a.map(* + 1) } #OK shadow
is sum([1, 2, 3]), 9, 'shadow body calling builtins resolves';

# &SETTING:: still reaches the builtin even with a user shadow present (PR-1 guard).
sub not($x) { "USER" } #OK shadow
is &SETTING::not(False), True, '&SETTING::not still uses builtin under a shadow';
