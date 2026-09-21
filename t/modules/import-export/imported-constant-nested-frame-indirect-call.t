use v6;
use lib 't/lib';
use Test;
use ImportConstUser;

# A `my sub` that reaches a sibling module's imported `constant` only as an
# argument passed to ANOTHER named `my sub` (`lookup-via-helper` -> `bisearch`,
# mirroring Terminal::WCWidth's `wcwidth` -> `bisearch(_, ZERO_WIDTH)`)
# resolved that constant correctly when reached directly by name, and even
# when called indirectly (`.map(&lookup-via-helper)`) from the *mainline*
# script body — but not when that same indirect call happened from within
# ANOTHER named sub's own call frame (`outer`, below).
#
# Root cause: `call_compiled_closure_with_topic` (the general dispatch for
# invoking a `Sub` value — `&name`, `.map(&name)`, a stored callback, ...)
# always pushed the callee's `RoutineFrame` with `is_block: true`, even for a
# genuine named/anonymous `sub` (as opposed to a bare/pointy block). A direct
# by-name call pushes `is_block: false` for the very same callee. Every
# consumer of `RoutineFrame.is_block` that walks the routine stack looking for
# "the nearest enclosing NAMED routine" (above all
# `Interpreter::running_package_candidates`, which anchors bareword/constant
# resolution) skipped straight past the misclassified callee frame and
# anchored on whatever real named routine happened to be calling it instead —
# `outer`, here, whose own module has no `use ImportConstTable` and so cannot
# see `TABLE` at all. Fixed by pushing `is_block: false` whenever the callee
# is a genuine routine (`cc.is_routine`), matching the by-name call path.
# See https://github.com/tokuhirom/mutsu/issues/8905

plan 3;

is lookup-via-helper(1), 3,
    "direct named call sees the constant reached only via a helper sub";
is (1, 2).map(&lookup-via-helper).join(","), "3,3",
    "an indirect call from the mainline body also resolves the constant";

sub outer($x) {
    (1, 2).map(&lookup-via-helper).join(",")
}
is outer(1), "3,3",
    "an indirect call from within another named sub's own frame also resolves the constant (#8905)";
