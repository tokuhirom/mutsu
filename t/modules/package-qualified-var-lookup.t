use Test;

# The VM's free-variable fallbacks that rebuild a package-qualified key from
# a bare name (or split one back apart) -- the paths #11507 moved onto the
# memoized `src/qualified.rs` constructors. Each case pins the key those
# paths must reconstruct.

plan 9;

# Bare read of an enclosing package's `our` from a nested class's method
# (package_chain_var_fallback: `M::R` -> `M`).
module M {
    our $greeting = 'hi';
    our @list = 1, 2, 3;
    class R {
        method greet { $greeting }
        method count { @list.elems }
    }
}
is M::R.new.greet, 'hi', 'bare scalar resolves through the enclosing package chain';
is M::R.new.count, 3, 'bare array resolves through the enclosing package chain';

# Read-modify-write of a package `our` from inside a named sub
# (read_package_scope_var / writeback_package_scope_var).
package Counter {
    our $n = 0;
    our sub bump { $n++; $n += 10; $n }
}
is Counter::bump(), 11, 'inc-dec and compound assignment reach the package our';
is $Counter::n, 11, 'the write landed in the package store';

# Nested package shorthand: `$D2::d3` from inside `D1::D2` finds `$D1::D2::d3`.
package D1 {
    package D2 {
        our $d3 = 'deep';
        our sub get { $D2::d3 }
    }
}
is D1::D2::get(), 'deep', 'partially qualified name resolves against the current package';

# Bare enum member read through the package chain from a method.
class EnumC {
    our enum HF <Lines Lists>;
    method pick { Lists }
}
is EnumC.new.pick, EnumC::Lists, 'bare enum member resolves through the declaring package';

# A sigiled free variable is also looked up under `Main::`.
package Main { our $mainvar = 'main'; }
is $Main::mainvar, 'main', 'Main-qualified our is readable';

# OUR:: pseudo-package inside a package resolves to that package's `our`.
package P {
    our $x = 'px';
    our sub ox { $OUR::x }
}
is P::ox(), 'px', 'OUR:: pseudo-package resolves to the current package';

# Class-body `my @a` read by a later body statement under its auto-qualified name.
class C {
    my @predef = <a b>;
    my $seen = @predef.elems;
    method seen { $seen }
}
is C.new.seen, 2, 'class-body my array is visible to a later body statement';
