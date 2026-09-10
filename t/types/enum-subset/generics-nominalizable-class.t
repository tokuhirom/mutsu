use v6.e.PREVIEW;
use Test;

# A class declared inside a parametric role body becomes parametric over the
# role's type args: its `.^name` is per-composition (`R::G::A[Int]`), and an
# attribute typed with it (`has @.a is G::A`) enforces the concrete element type.

plan 11;

my role R[::T] {
    my package G {
        class A is Array[T] {}
    }
    has @.a is G::A;
    method a-nominalizables { G::A:D, G::A:U, G::A(), G::A:D() }
}

my class CInt does R[Int] { }
my class CStr does R[Str] { }

is CInt.a-nominalizables.map(*.^name).List,
   ("R::G::A[Int]:D", "R::G::A[Int]:U", "R::G::A[Int](Any)", "R::G::A[Int]:D(Any)"),
   "generic class instantiates over Int";
is CStr.a-nominalizables.map(*.^name).List,
   ("R::G::A[Str]:D", "R::G::A[Str]:U", "R::G::A[Str](Any)", "R::G::A[Str]:D(Any)"),
   "generic class instantiates over Str";

# Typed-attribute element checking flows through the generic class's Array[T] base.
lives-ok { CInt.new(a => (1, 10, 20)) }, "CInt accepts Int elements";
throws-like { CInt.new(a => <A B C>) }, X::TypeCheck::Assignment,
    "CInt rejects Str elements";
lives-ok { CStr.new(a => <A B C>) }, "CStr accepts Str elements";
throws-like { CStr.new(a => (1, 2)) }, X::TypeCheck::Assignment,
    "CStr rejects Int elements";

# The plain (non-generic) `is Array[Int]` container trait keeps its
# parameterization: the `[Int]` is not dropped as a separate statement.
class Plain { has @.nums is Array[Int] }
is Plain.new(a => Nil).defined, True, "Plain constructs";
throws-like { Plain.new(nums => <x y>) }, X::TypeCheck::Assignment,
    "is Array[Int] rejects wrong element types at construction";
my $p = Plain.new(nums => [1, 2, 3]);
is $p.nums.List, (1, 2, 3), "is Array[Int] keeps good values";
is $p.nums.^name, "Array[Int]", "is Array[Int] tags the container type";

# A `::`-qualified `is` container trait name is parsed whole.
class Box { has @.items is Array[Int] }
lives-ok { Box.new(items => [4, 5]) }, "qualified/parameterized is-type parses";
