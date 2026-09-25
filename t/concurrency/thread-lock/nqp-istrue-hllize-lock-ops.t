use v6;
use Test;
use nqp;

# nqp::istrue, nqp::islist, nqp::hllize, nqp::what, nqp::lock/nqp::unlock, and
# the *_nd ("no decontainerize") siblings of nqp::istype/nqp::isconcrete/
# nqp::clone. Every operand nqp_ops.rs sees is already decontainerized once at
# the `call_nqp_op` boundary, so the *_nd forms share their base op's
# implementation. Driven by AttrX::Mooish (ecosystem/dists/A/AttrX--Mooish),
# whose Attribute/ClassHOW subclass the real Metamodel `Attribute` and lean on
# these directly rather than going through `?`/`.WHAT`/`Lock.lock`.

plan 17;

# -- nqp::istrue: nqp's own truthiness, distinct from Raku's `so` -----------

is nqp::istrue(0), 0, 'nqp::istrue is 0 for int 0';
is nqp::istrue(1), 1, 'nqp::istrue is 1 for a nonzero int';
is nqp::istrue(""), 0, 'nqp::istrue is 0 for an empty string';
is nqp::istrue("0"), 1, 'nqp::istrue is 1 for the string "0" (unlike Raku boolification)';

# -- nqp::islist: a raw nqp list, not a boxed value -------------------------

my $l := nqp::list(1, 2, 3);
is nqp::islist($l), 1, 'nqp::islist is 1 for an nqp::list()';
is nqp::islist(42), 0, 'nqp::islist is 0 for a plain int';

# -- nqp::hllize: identity (mutsu has no separate nqp/HLL representation) ---

is nqp::hllize(42), 42, 'nqp::hllize is the identity for an int';
is nqp::hllize("abc"), "abc", 'nqp::hllize is the identity for a string';

# -- nqp::what: the type object, like `.WHAT` -------------------------------

is nqp::what(42).^name, 'Int', 'nqp::what(42) is the Int type object';
is nqp::what("x").^name, 'Str', 'nqp::what("x") is the Str type object';

# -- nqp::lock / nqp::unlock: the Lock.lock/.unlock critical section --------
# (through a bound attribute, matching AttrX::Mooish's `$!lock is
# built(:bind) = Lock.new` — a *containerized* Lock hits a REPR mismatch in
# real nqp, which is not the shape this op needs to support.)

class HasLock {
    has $!lock is built(:bind) = Lock.new;
    method locked-count {
        nqp::lock($!lock);
        my $n = 1;
        nqp::unlock($!lock);
        $n;
    }
}
is HasLock.new.locked-count, 1, 'nqp::lock/nqp::unlock round-trip on a bound Lock attribute';

# -- the *_nd siblings: same operand shape as their base op -----------------

class Foo { }
my $f := Foo.new;
is nqp::istype_nd($f, Foo), 1, 'nqp::istype_nd matches like nqp::istype';
is nqp::isconcrete_nd($f), 1, 'nqp::isconcrete_nd is 1 for a concrete instance';
is nqp::isconcrete_nd(Foo), 0, 'nqp::isconcrete_nd is 0 for the type object';

my $l2 := nqp::clone_nd($l);
nqp::push($l2, 4);
is nqp::elems($l), 3, 'nqp::clone_nd leaves the original list untouched';
is nqp::elems($l2), 4, 'and the clone can grow independently';

# -- nqp::iscont --------------------------------------------------------------
# Was pinned here as an explicit "Unsupported" gap; #9346 implemented it (the
# operand is compiled as `.VAR`). Full coverage: t/vm/nqp-iscont-where.t.

my $cont = 1;
is nqp::iscont($cont) ~ nqp::iscont(1), '10', 'nqp::iscont tells a container from a bare value';
