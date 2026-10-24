use v6;
use Test;

plan 7;

# A routine's `$_` is its own: a typed `$_` parameter of the CALLER does not
# constrain what the callee assigns to its topic.
class Foo { }
class Bar { }

sub plain-inner { $_ = 5; $_ }
sub typed-outer(Foo $_) { plain-inner() }
is typed-outer(Foo.new), 5, "a sub that assigns its own topic under a typed-$_ caller";

sub str-outer(Str $_) { plain-inner() }
is str-outer("a"), 5, "a Str-typed caller topic does not apply to the callee";

class K {
    multi method inner(Bar $x) { (1, 2).map(-> $e { $_ = $e * 10; $_ }).join(",") }
    multi method outer(Foo $_) { self.inner(Bar.new) }
}
is K.new.outer(Foo.new), "10,20", "a method assigning its topic, called from a typed-$_ method";

sub show { $_.defined ?? "defined" !! "undefined" }
sub outer-show(Str $_) { show() }
is outer-show("a"), "undefined", "the callee's topic starts empty, not the caller's";

# The caller's own constraint is still enforced after the callee returned.
sub keeps(Str $_) { my $r = plain-inner(); try { $_ = 7; CATCH { default { return "caught" } } }; "no error" }
is keeps("x"), "caught", "the caller's typed topic still refuses a wrong assignment";

# An untyped topic is unaffected.
for 1 { plain-inner(); }
is $_.defined ?? "defined" !! "undefined", "undefined", "the mainline topic is not touched by the callee";

sub nested(Str $_) { my sub in2 { $_ = 3; $_ }; in2() ~ ":" ~ $_ }
is nested("z"), "3:z", "a nested sub's own topic does not leak into the enclosing typed topic";
