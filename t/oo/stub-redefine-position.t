use v6;
use Test;

# A yada-stub routine is a forward declaration: redefining it without
# `supersede` is allowed regardless of where the pair sits in the file.
# This used to be position-sensitive (the stub's in-place re-registration
# died with X::Redeclaration once the real definition had replaced it,
# unless unrelated later file content changed the hoist/pragma state) —
# todo/tickets/stub-redeclaration-is-position-sensitive.md. Expected
# values verified against raku.

plan 5;

sub lightning {...}
sub lightning {42}
is lightning(), 42, "stub then definition at the top level";

{
    sub inner {...}
    sub inner {7}
    is inner(), 7, "stub then definition inside a bare block";
}

sub spaced {...}
my $mid = "between";
sub spaced {9}
is spaced(), 9, "a statement between stub and definition";
is $mid, "between", "the in-between statement still ran";

# Multiple stubs before the definition are fine (raku: 42).
sub multi-stubbed {...}
sub multi-stubbed {...}
sub multi-stubbed {26}
is multi-stubbed(), 26, "double stub then definition";

# NOTE: a stub textually AFTER the definition is a compile-time
# X::Redeclaration in raku. mutsu raises it at registration time, which
# eval-dies-ok cannot observe portably across hoist passes, so that arm
# is pinned by the shapes above staying errors in -e form.

done-testing;
