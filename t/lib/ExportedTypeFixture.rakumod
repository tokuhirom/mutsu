unit class ExportedTypeFixture;

# A class exported under the DEFAULT tag (bare `is export`); also visible
# under `:ALL`, matching Rakudo's `is export` == `is export(:DEFAULT, :ALL)`.
class DefaultCls is export {
    method who { "DefaultCls" }
}

# A class exported ONLY under the :ALL tag — must NOT leak into a plain `use`.
class AllOnlyCls is export(:ALL) {
    method who { "AllOnlyCls" }
}

# Two `multi method new` candidates distinguished only by param sigil:
# `(@x, @y)` must win over `($a, $b)` for two array arguments (the sigil
# imposes an implicit Positional constraint, narrower than a bare Any).
class Model is export(:ALL) {
    has $.tag;
    multi method new(@x, @y) { self.bless(:tag<arrays>) }
    multi method new($a, $b) { self.bless(:tag<scalars>) }
}
