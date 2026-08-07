use Test;
use MONKEY-TYPING;

# ADR-0019 D3-5: `augment class` now mirrors the class/role walkers' handling
# of `my method` / `our method` (not part of the method table; registered as
# functions instead) and privacy-aware duplicate-method detection (a private
# and a public method of the same name coexist). Verified against raku.

plan 8;

# `my method` inside augment: not in the method table, but callable
# lexically from a sibling method declared in the same augment block.
{
    class LexMethod { }
    augment class LexMethod {
        my method secret { 'secret' }
        method pub { secret(self) }
    }
    is LexMethod.new.can('secret').elems, 0, 'my method is not in the method table';
    is LexMethod.new.pub, 'secret', 'my method is callable lexically from a sibling method';
}

# `our method` inside augment: not in the method table, but callable as a
# package-qualified sub.
{
    class PkgMethod { }
    augment class PkgMethod {
        our method pkg { 'pkg' }
    }
    is PkgMethod.new.can('pkg').elems, 0, 'our method is not in the method table';
    is PkgMethod::pkg(PkgMethod.new), 'pkg', 'our method is callable as Package::name(invocant)';
    dies-ok { PkgMethod.new.pkg }, 'our method is not callable as a method';
}

# Privacy-aware duplicate detection: a public and a private method of the
# same name coexist (separate namespaces), matching the class/role walkers.
{
    class PrivPub {
        method pub { 'pub' }
    }
    augment class PrivPub {
        method !pub { 'priv-pub' }
    }
    is PrivPub.new.pub, 'pub', 'public method unaffected by a same-named private augment method';
}

# Same-privacy redeclaration across class + augment is still rejected.
{
    class SamePriv {
        method !priv { 'one' }
    }
    dies-ok {
        EVAL q[
            use MONKEY-TYPING;
            augment class SamePriv {
                method !priv { 'two' }
            }
        ];
    }, 'redeclaring the same private method via augment still errors';
}

# Multi methods declared via augment still coexist with the original.
{
    class MultiAugment {
        multi method greet(Int $x) { "int:$x" }
    }
    augment class MultiAugment {
        multi method greet(Str $x) { "str:$x" }
    }
    is MultiAugment.new.greet("hi"), 'str:hi', 'multi method added via augment still dispatches';
}
