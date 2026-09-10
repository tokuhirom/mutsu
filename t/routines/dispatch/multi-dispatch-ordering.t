use Test;

# Method/multi dispatch ordering: explicit method-literal invocants, `nextsame`
# into a built-in constructor, and attribute enumeration order under multiple
# inheritance. Every expectation below was taken from rakudo v2026.06.

plan 27;

# ---------------------------------------------------------------------------
# A method literal may name its invocant (`method ($x: $p) { ... }`). The named
# invocant is NOT an extra positional -- it aliases the receiver, and `self`
# keeps working alongside it.
# ---------------------------------------------------------------------------

{
    my $m = method ($invocant: $param) { "$invocant/$param" };
    is "greeting".$m("hello"), "greeting/hello", 'named invocant + positional param';
}

{
    my $m = method ($x:) { $x };
    is 5.&$m, 5, 'named invocant with no other params';
}

{
    my $m = method ($x:) { self ~ "|" ~ $x };
    is "a".&$m, "a|a", '`self` still bound alongside a named invocant';
}

{
    my $m = method ($x: $a, $b) { $x + $a + $b };
    is 1.&$m(2, 3), 6, 'named invocant with two positionals';
}

{
    my $m = method (Int $x: $p) { $x * $p };
    is 5.&$m(3), 15, 'typed named invocant binds and type-checks';
}

# `.^add_method` with a method literal that names its invocant.
{
    Int.^add_method('mutsu-double', method ($x:) { 2 * $x });
    is 21.mutsu-double, 42, 'add_method with a named invocant';
}

# ---------------------------------------------------------------------------
# A type-only invocant declaration (`method (List:D:)`) is legal and constrains
# the invocant; the invocant binds raw, so `self` is not itemized.
# ---------------------------------------------------------------------------

{
    my $m = method (List:D:) { self.raku };
    is <a b c>.&$m, '("a", "b", "c")', 'type-only invocant, self bound raw (not itemized)';
}

{
    my $m = method (Int:D:) { "ok" };
    dies-ok { "s".&$m }, 'type-only invocant type-check rejects a bad invocant';
}

# The same shape used as a term with a `my` declarator.
{
    is <a b c>.&(my method (List:D:) { self.elems }), 3,
        '`my method (...)` usable as an expression term';
}

{
    is 21.&(my method { self * 2 }), 42, '`my method { ... }` usable as an expression term';
}

# A method literal with no explicit invocant is unchanged.
{
    my $m = method ($p) { self ~ $p };
    is "x".&$m("y"), "xy", 'implicit invocant still works';
}

{
    my $m = method { self.raku };
    is <a b>.&$m, '("a", "b")', 'implicit invocant binds raw too';
}

# ---------------------------------------------------------------------------
# Attribute enumeration order: `.^attributes`, `.raku` and the default `.gist`
# all walk the MRO forwards -- most-derived class first, each class's own
# attributes in declaration order.
# ---------------------------------------------------------------------------

class Bull        { has Bool $.castrated = False; }
class Automobile  { has $.direction; }
class Taurus is Bull is Automobile { }

is Taurus.new.gist, 'Taurus.new(castrated => Bool::False, direction => Any)',
    'multiple-inheritance gist lists parents in MRO order';
is Taurus.new.raku, 'Taurus.new(castrated => Bool::False, direction => Any)',
    'multiple-inheritance raku lists parents in MRO order';
is Taurus.^attributes.map(*.name).join(' '), '$!castrated $!direction',
    '.^attributes agrees with the gist order';

class AA { has $.a = 1; }
class BB { has $.b = 2; }
class CC is AA is BB { has $.c = 3 }

is CC.new.raku, 'CC.new(c => 3, a => 1, b => 2)',
    'own attributes come before inherited ones';
is CC.^attributes.map(*.name).join(' '), '$!c $!a $!b',
    '.^attributes agrees for own-before-inherited';

class DD { has $.d = 1; has $.e = 2 }
class EE is DD { has $.f = 3 }

is EE.new.raku, 'EE.new(f => 3, d => 1, e => 2)',
    'single inheritance: own attribute first, parent declaration order preserved';

# ---------------------------------------------------------------------------
# `nextsame` from a user `.new` override reaches the nearest built-in ancestor's
# native constructor, not the generic `Mu.new`/`bless`.
# ---------------------------------------------------------------------------

{
    my @log;
    class LoggedVersion is Version {
        method new(|c) {
            @log.push: 'called';
            nextsame;
        }
    }
    my $v = LoggedVersion.new('1.0.2');
    is @log.join(','), 'called', 'the user new override ran';
    is $v.gist, 'v1.0.2', 'nextsame reached the built-in Version constructor';
    is $v.parts.join('.'), '1.0.2', 'the result is a real Version';
}

{
    class CallsameVersion is Version {
        method new(|c) { my $r = callsame(); $r }
    }
    is CallsameVersion.new('2.5').gist, 'v2.5', 'callsame reaches the built-in constructor too';
}

{
    class IntSub is Int {
        method new(|c) { nextsame }
    }
    is IntSub.new(7), 7, 'nextsame into the built-in Int constructor';
}

# A plain user class (no built-in ancestor) still falls through to bless.
{
    class Plain {
        has $.x;
        method new(|c) { nextsame }
    }
    is Plain.new(x => 5).raku, 'Plain.new(x => 5)', 'plain class nextsame still blesses';
}

# ---------------------------------------------------------------------------
# `is default` only breaks ties between equally narrow candidates; a narrower
# candidate still wins outright.
# ---------------------------------------------------------------------------

{
    my multi mdo-f($a) is default { 'default' }
    my multi mdo-f(Int $a)        { 'Int' }
    is mdo-f(1), 'Int', 'a narrower candidate beats an `is default` one';
    is mdo-f('s'), 'default', 'the `is default` candidate still catches the rest';
}

{
    my multi mdo-g(Int $a) is default { 'A' }
    my multi mdo-g(Int $a)            { 'B' }
    is mdo-g(1), 'A', '`is default` breaks a tie between equally narrow candidates';
}
