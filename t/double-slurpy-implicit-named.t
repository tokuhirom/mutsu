use Test;

# A `**@values` (double-star / slip-preserving) slurpy is a POSITIONAL slurpy,
# not a named one, so it must NOT suppress the implicit `*%_` that every method
# otherwise receives. Without it `%_` was `Any` (not an empty Hash), and
# `self.bless(|%_)` splatted a stray `Any` positional into TWEAK/BUILD, dying
# with "Too many positionals passed; expected 0 arguments but got 1".

plan 6;

# `%_` inside a method with a `**@values` slurpy is an (empty) Hash.
class A {
    method m(**@v) { %_ }
}
is-deeply A.new.m, {}, '%_ is an empty Hash under a **@ slurpy';
isa-ok A.new.m, Hash, '%_ is a Hash, not Any';

# The Hash::Agnostic construction shape: a custom `new` with `**@values is raw`
# that forwards `|%_` to `bless`, plus a `submethod TWEAK`.
class B {
    has $.x;
    method new(::?CLASS:U: **@values is raw) { self.bless(|%_) }
    submethod TWEAK { }
}
ok B.new.defined, '.new with **@values + TWEAK constructs (no stray Any)';
is B.new(x => 42).x, 42, 'named args still bind through |%_';

# A genuine named slurpy (`*%opts` / `**%opts`) still consumes the implicit `%_`.
class C {
    has %.opts;
    method new(*%opts) { self.bless(:%opts) }
}
is-deeply C.new(a => 1, b => 2).opts, {a => 1, b => 2}, '*%named slurpy captures named args';

# `**@values` collects the positionals (slip-preserving) and `%_` the nameds.
class D {
    method split(**@v) { (@v, %_) }
}
is-deeply D.new.split(1, 2, :k<v>), ([1, 2], {k => 'v'}),
    '**@ takes positionals, implicit %_ takes nameds';
