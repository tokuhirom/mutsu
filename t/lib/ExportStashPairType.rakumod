my role Callable-Type {
    method CALL-ME(Callable-Type:U: Str:D $name) { "called {self.^name}($name)" }
}
my class Lic does Callable-Type { }
my class Ack does Callable-Type { }

# The SBOM::enums shape: export the unit's own `my` classes picked from
# `UNIT::`, as stash pairs.
my sub EXPORT(*@names) {
    @names ||= UNIT::.map({ .key if .value ~~ Callable-Type });
    Map.new: @names.map: { UNIT::{$_}:p }
}
