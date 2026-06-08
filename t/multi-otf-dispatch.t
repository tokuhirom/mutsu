use Test;

# Pin for ③ §2: non-proto multi candidates are resolved VM-side
# (resolve_function_with_types) and the unambiguous, OTF-compilable winner runs
# as compiled bytecode instead of tree-walking through call_function_fallback.
# Regression-guards the cases that must stay on the interpreter: ambiguity,
# no-match, and redispatch (nextsame/callsame/callwith) correctness.

plan 24;

# --- type dispatch ---
{
    multi sub f(Int $x) { "int:$x" }
    multi sub f(Str $x) { "str:$x" }
    is f(5),    'int:5', 'multi type dispatch: Int';
    is f("hi"), 'str:hi', 'multi type dispatch: Str';
    # repeated call with a different type must NOT reuse a cached candidate
    is f(7),    'int:7', 'multi: no name-cache pollution (Int after Str)';
    is f("yo"), 'str:yo', 'multi: no name-cache pollution (Str again)';
}

# --- value dispatch ---
{
    multi sub g(0)      { 'zero' }
    multi sub g(Int $n) { "n:$n" }
    is g(0), 'zero', 'multi value dispatch: literal 0';
    is g(3), 'n:3',  'multi value dispatch: general Int';
}

# --- subclass type-distance ---
{
    class Animal { }
    class Dog is Animal { }
    multi sub h(Animal $a) { 'animal' }
    multi sub h(Dog $d)    { 'dog' }
    is h(Dog.new),    'dog',    'multi narrowest by MRO distance';
    is h(Animal.new), 'animal', 'multi base class';
}

# --- return value / arithmetic in compiled candidate ---
{
    multi sub add(Int $a, Int $b) { $a + $b }
    multi sub add(Str $a, Str $b) { $a ~ $b }
    is add(3, 4),     7,    'multi compiled body arithmetic';
    is add("a", "b"), 'ab', 'multi compiled body concat';
}

# --- nextsame redispatches to next candidate ---
{
    multi sub k(Int $x) { 'k-int(' ~ nextsame() ~ ')' }
    multi sub k(Any $x) { 'k-any' }
    is k(42), 'k-any', 'nextsame redispatch from compiled candidate';
}

# --- callsame redispatches and returns ---
{
    multi sub m(Int $x) { 'm-int+' ~ callsame() }
    multi sub m(Any $x) { 'm-any' }
    is m(9), 'm-int+m-any', 'callsame redispatch from compiled candidate';
}

# --- callwith redispatches with new args ---
{
    multi sub w(Int $x) { 'w-int:' ~ callwith(100) }
    multi sub w(Any $x) { "w-any:$x" }
    is w(1), 'w-int:w-any:100', 'callwith redispatch with new args';
}

# --- is default tie-break ---
{
    multi sub dd(Int $x) is default { 'default' }
    multi sub dd($x)                { 'generic' }
    is dd(5), 'default', 'is default tie-break';
}

# --- ambiguity raises X::Multi::Ambiguous ---
{
    multi sub amb(Int $a, $b) { 'A' }
    multi sub amb($a, Int $b) { 'B' }
    my $caught;
    { amb(1, 2); CATCH { default { $caught = .^name } } }
    is $caught, 'X::Multi::Ambiguous', 'ambiguous multi raises X::Multi::Ambiguous';
}

# --- no-match raises X::Multi::NoMatch (runtime arg avoids compile-time check) ---
{
    multi sub nm(Int $x) { 'int' }
    my $arg = "string";
    my $caught;
    { nm($arg); CATCH { default { $caught = .^name } } }
    is $caught, 'X::Multi::NoMatch', 'no matching multi raises X::Multi::NoMatch';
}

# --- where-constrained candidate still dispatched correctly (stays on interpreter) ---
{
    multi sub wc(Int $x where * > 10) { 'big' }
    multi sub wc(Int $x)              { 'small' }
    is wc(20), 'big',   'where-constrained multi: matches';
    is wc(5),  'small', 'where-constrained multi: falls to general';
}

# --- optional / slurpy candidates ---
{
    multi sub o(Int $x)        { "one:$x" }
    multi sub o(Int $x, Int $y) { "two:$x,$y" }
    is o(1),    'one:1',   'multi by arity 1';
    is o(1, 2), 'two:1,2', 'multi by arity 2';
}

# --- named-parameter dispatch ---
{
    multi sub n(:$a!) { "a:$a" }
    multi sub n(:$b!) { "b:$b" }
    is n(a => 1), 'a:1', 'multi named dispatch a';
    is n(b => 2), 'b:2', 'multi named dispatch b';
}

# --- state shared across signature alternates (must stay on interpreter) ---
{
    multi sub sc(Int $x) { state $c = 0; ++$c }
    multi sub sc(Str $x) { state $c = 0; ++$c }
    # NOTE: distinct bodies => distinct per-candidate state (not shared).
    is sc(1), 1, 'distinct-body multi: per-candidate state (Int)';
    is sc(2), 2, 'distinct-body multi: per-candidate state persists (Int)';
}
