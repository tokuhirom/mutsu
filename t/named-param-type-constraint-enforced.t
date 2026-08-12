use Test;

plan 10;

# A named `:$param`'s declared type constraint used to be silently ignored
# at binding — the positional path did the full check (built-in types,
# coercion, subset `where` clauses), but the named path bound the raw value
# unconditionally. See
# todo/tickets/named-parameter-user-subset-type-not-enforced-at-binding.md.

sub int-named(Int :$x!) { "ok $x" }
lives-ok { int-named(x => 5) }, "named Int param accepts a matching Int";
dies-ok { int-named(x => "not an int") }, "named Int param rejects a Str";

subset UUIDv4 of Str where /^ <[0..9a..f]> ** 8 $/;
sub subset-named(UUIDv4 :$id!) { "ok $id" }
lives-ok { subset-named(id => "deadbeef") }, "named subset param accepts a matching value";
dies-ok { subset-named(id => "not-a-uuid") }, "named subset param rejects a where-mismatch";

class Point { has $.x; has $.y; }
sub class-named(Point :$p!) { "ok" }
lives-ok { class-named(p => Point.new) }, "named user-class param accepts an instance";
dies-ok { class-named(p => 42) }, "named user-class param rejects a mismatched type";

# Optional named params, defaults, and untyped/@/% named params are
# unaffected (no type_constraint means the new check is a no-op).
sub optional-named(Int :$y = 5) { $y }
is optional-named(), 5, "unsupplied optional named param still uses its default";
is optional-named(y => 10), 10, "supplied optional named param still binds normally";

sub array-named(:@list) { @list.elems }
is array-named(list => [1, 2, 3]), 3, "untyped named array param still binds normally";

# An unsupplied `&`-sigil named param's implicit type is `Callable`, not
# `Any` — otherwise a downstream `Callable :$x` constructor param (as in
# Template::Mustache's Logger, which passes an unsupplied `:&log-routine`
# into `Logger.new(:routine(&log-routine))`) rejects it as a Str/Any mismatch.
sub amp-named(:&cb) { &cb ~~ Callable }
ok amp-named(), "an unsupplied &-sigil named param's default type-checks as Callable";
