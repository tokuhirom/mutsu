use v6;
use Test;
use lib $*PROGRAM.parent.add('lib');
use ClosureShadowCallee;

# A closure passed as a NAMED argument to an imported routine must keep its own
# captured lexical, even when the routine invokes it from a nested block that
# itself reads a same-named parameter. See
# news/2026-08/closure-capture-shadowed-by-colliding-callee-parameter.md.
#
# NOTE: a second sibling top-level block declaring its own `$s` is part of the
# repro, not incidental -- dropping it stopped the bug from reproducing during
# investigation (`box_captured_lexicals`'s escape analysis is name-based across
# the whole compiled unit).

plan 2;

{
    my $s = Supplier.new;
    closure-shadow-callee $s.Supply, [1, 2, 3], :after({
        $s.emit(1);
        $s.emit(2);
        $s.emit(3);
        $s.done;
    });
}

{
    my $s = 99;
    ok $s == 99, "unrelated same-named sibling block";
}

done-testing;
