use Test;

# A class-scoped scratch-variable idiom: `my (\x1, \x2) := my ($x1, $x2);` (or
# the single-variable `my $x1; my \x1 := $x1;`) placed directly at the top
# level of a class body, then written to from a method. This is how zef's
# Math::Interval 0.0.3 shares per-instance scratch scalars across its
# `Operation` helper class's `TWEAK` and its arithmetic methods.
#
# `Compiler::class_body_plan`'s flatten step used to split every top-level
# `SyntheticBlock` into its individual inner statements, each compiled as its
# own ISOLATED one-statement chunk (a fresh child `Compiler`, per ADR-0019
# D6-3b's "Other" statement handling). A sigilless bind lowers to
# `SyntheticBlock([MarkBind, VarDecl, MarkSigilless])`: splitting it apart lost
# the sibling relationship the compiler's own `Stmt::SyntheticBlock` arm needs
# to see `MarkBind`/`MarkSigilless` next to the `VarDecl` at compile time. The
# `VarDecl`'s compile no longer emitted `MarkSigillessBindSource`, so the
# runtime's `OpCode::MarkSigillessBind` fell back to inspecting the
# already-dereferenced stored value (a plain `Int`) instead of the compiler's
# verdict, and marked the class-scoped scratch variable permanently readonly —
# the first write from ANY method then died with "Cannot modify an immutable
# Int". Reproduced and fixed via `tokuhirom/mutsu`#7884's lock board
# (ecosystem-dist-roulette on Math::Interval).

plan 2;

{
    class Holder1 {
        my $x1;
        my \x1 := $x1;
        method set(\v) { x1 = v }
        method get() { x1 }
    }
    Holder1.set(5);
    is Holder1.get(), 5,
        'single-variable class-body sigilless bind (`my \x := $x;`) stays writable from a method';
}

{
    my class Scratch {
        my (\x1, \x2, \y1, \y2) := my ($x1, $x2, $y1, $y2);
        has $.x;
        has $.y;
        submethod TWEAK {
            ($x1, $x2) = ($!x, $!x + 1);
            ($y1, $y2) = ($!y, $!y + 1);
        }
        method sum { x1 + x2 + y1 + y2 }
    }
    my $o = Scratch.new(x => 5, y => 10);
    is $o.sum, 5 + 6 + 10 + 11,
        'grouped class-body sigilless bind (`my (\a,\b) := my ($a,$b);`) stays writable from TWEAK/methods';
}
