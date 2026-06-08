use Test;

# Pin for ③ §1: a resolved user method that lacks pre-compiled bytecode (e.g.
# added via `.^add_multi_method`, which hardcodes compiled_code = None) is
# compiled on demand at the dispatch chokepoint and run as bytecode, instead of
# falling through to the interpreter bridge. Regression-guards the common
# already-compiled cases (normal/multi/submethod/role/inheritance/.^add_method)
# at the same time.

plan 14;

# --- .^add_method: method literal carries its own compiled bytecode ---
{
    class C { has $.x = 10; }
    C.^add_method('double', method () { self.x * 2 });
    is C.new(x => 21).double, 42, '.^add_method runtime method runs correctly';
}

# --- .^add_multi_method: compiled_code starts None -> compile-on-demand ---
{
    class M { }
    M.^add_multi_method('f', method (Int $x) { "int:$x" });
    M.^add_multi_method('f', method (Str $x) { "str:$x" });
    M.^compose;
    is M.new.f(5),    'int:5', '.^add_multi_method dispatches Int candidate';
    is M.new.f('hi'), 'str:hi', '.^add_multi_method dispatches Str candidate';
    # call repeatedly to exercise the cached/registry-populated fast path
    is M.new.f(7),    'int:7', '.^add_multi_method repeat call (cached) still correct';
}

# --- normal declared method (compiled at registration) ---
{
    class E { method g { 99 } }
    is E.new.g, 99, 'normal declared method';
}

# --- declared multi method (all candidates compiled at registration) ---
{
    class MM {
        multi method h(Int $x) { "i$x" }
        multi method h(Str $x) { "s$x" }
    }
    is MM.new.h(3),   'i3', 'declared multi method Int';
    is MM.new.h('z'), 'sz', 'declared multi method Str';
}

# --- submethod ---
{
    class S { submethod sm { 'sub-method' } }
    is S.new.sm, 'sub-method', 'submethod dispatch';
}

# --- role composition ---
{
    role R { method rm { 'role-method' } }
    class CR does R { }
    is CR.new.rm, 'role-method', 'role-composed method dispatch';
}

# --- inheritance ---
{
    class Base { method bm { 'base' } }
    class Derived is Base { }
    is Derived.new.bm, 'base', 'inherited method dispatch';
}

# --- mutating method via add_method (mut receiver path) ---
{
    class Counter { has $.n is rw = 0; method bump { $!n++ } }
    my $c = Counter.new;
    $c.bump; $c.bump; $c.bump;
    is $c.n, 3, 'mutating method on variable receiver';
}

# --- add_method on built-in-ish class returns correct result on call ---
{
    class P { has $.v = 1; }
    P.^add_method('triple', method () { self.v * 3 });
    is P.new(v => 4).triple, 12, '.^add_method with attribute access';
}

# --- attribute mutation persists across add_multi_method dispatch ---
{
    class Acc { has @.log; }
    Acc.^add_multi_method('push-log', method (Int $x) { self.log.push($x); self.log.elems });
    Acc.^compose;
    my $a = Acc.new;
    is $a.push-log(1), 1, 'add_multi_method attribute mutation 1';
    is $a.push-log(2), 2, 'add_multi_method attribute mutation 2';
}
