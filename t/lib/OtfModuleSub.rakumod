unit module OtfModuleSub;

# Non-builtin module subs exercised by t/module-sub-otf-dispatch.t. These are
# resolved through the `user_function_matches_call` branch in exec_call_func_op
# (they are not in the importing scope's compiled_fns), so before the §D
# multi-dispatch OTF slice they always tree-walked. They must now OTF-compile to
# bytecode and stay byte-identical, including default params (name-cache-safe
# because no same-named builtin can be mis-bound).

# Plain single sub, no default.
sub plain($name, $greeting) is export { "$greeting, $name" }

# Single sub WITH a default param (the case the strict gate excluded).
sub greet($name, $greeting = "Hello") is export { "$greeting, $name" }

# Two default params.
sub joiner($a, $b = "-", $c = "x") is export { "$a$b$c" }

# Recursion through the module sub itself.
sub fac(Int $n, $acc = 1) is export { $n <= 1 ?? $acc !! fac($n - 1, $acc * $n) }

# A body that calls builtins and uses control flow (but no nested routine decl).
sub classify($n) is export {
    if $n %% 2 { "even" } else { "odd" }
}

# A nested routine decl: must KEEP tree-walking (gate excludes it).
sub with-nested($x) is export {
    sub helper($y) { $y * 10 }
    helper($x) + 1
}
