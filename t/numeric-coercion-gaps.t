use v6;
use Test;

# Regression pins for a batch of numeric / coercion divergences. Every case
# below asserts the *type* as well as the value: several of these bugs produced
# a right-looking number of the wrong type.

plan 67;

# --------------------------------------------------------------------------
# 1. Generic `Real` arithmetic goes through `.Bridge` on BOTH operands.
#
# Rakudo's fallback candidate is `multi sub infix:<+>(Real \a, Real \b) {
# a.Bridge + b.Bridge }`, and every built-in numeric type's `.Bridge` is
# `self.Num`. So two user `Real`s whose `Bridge` returns a `Rat` add to an
# exact `Rat`, but mixing one with a built-in `Int`/`Rat` yields a `Num`.
# --------------------------------------------------------------------------

class T is Real {
    has $.v;
    method new($v) { self.bless(:$v) }
    method Bridge { $!v }
}

is (3.Bridge).WHAT, Num, 'Int.Bridge is a Num';
is ((1/2).Bridge).WHAT, Num, 'Rat.Bridge is a Num';

my $half    = T.new(1/2);
my $quarter = T.new(1/4);

is ($half + $quarter).WHAT, Rat, 'Real-subclass + Real-subclass stays exact';
is ($half + $quarter), 0.75, 'Real-subclass + Real-subclass value';
is ($half - $quarter).WHAT, Rat, 'Real-subclass - Real-subclass stays exact';
is ($half * $quarter).WHAT, Rat, 'Real-subclass * Real-subclass stays exact';

is ($half + 1/4).WHAT, Num, 'Real-subclass + Rat bridges the Rat to Num';
is ((1/4) + $half).WHAT, Num, 'Rat + Real-subclass bridges the Rat to Num';
is ($half + 1).WHAT, Num, 'Real-subclass + Int bridges the Int to Num';
is ($half + 1e0).WHAT, Num, 'Real-subclass + Num is a Num';
is ($half + 1/4), 0.75, 'Real-subclass + Rat value';

# Two Bridges that both return Int keep the sum an Int.
is (T.new(1) + T.new(2)).WHAT, Int, 'Int-bridging Real subclasses add to an Int';
is (T.new(1) + T.new(2)), 3, 'Int-bridging Real subclasses add correctly';

# A `Real` subclass is added through `Bridge` even when it also has `Numeric`.
class TN is Real {
    has $.v;
    method new($v) { self.bless(:$v) }
    method Bridge { $!v }
    method Numeric { 999 }
}
is (TN.new(1/2) + TN.new(1/4)), 0.75, 'Real subclass adds via Bridge, not Numeric';

# A plain (non-Real) object with `method Numeric` is NOT part of that rule:
# it numifies through `.Numeric` and leaves the other operand exact.
class F { method Numeric { 1/2 } }
is (F.new + 1/4).WHAT, Rat, 'non-Real object with Numeric keeps the other operand exact';
is (F.new + 1/4), 0.75, 'non-Real object with Numeric adds correctly';

# The doc's Temperature example: the running sum turns Num once a bridged Rat
# meets a further Real subclass.
class Temperature is Real {
    has Str:D  $.unit  is required where any <K F C>;
    has Real:D $.value is required;
    method new($value, :$unit = 'K') { self.bless :$value :$unit }
    method Bridge {
        when $!unit eq 'F' { ($!value + 459.67) * 5/9 }
        when $!unit eq 'C' {  $!value + 273.15 }
        $!value
    }
}
my $sum = Temperature.new(36.6, :unit<C>)
        + Temperature.new(451, :unit<F>)
        + Temperature.new(5778, :unit<K>);
is $sum.WHAT, Num, 'chained heterogeneous Real subclass sum is a Num';
is-approx $sum, 6593.677777777778, 'chained Real subclass sum value';

# --------------------------------------------------------------------------
# 2. The `[op]` reduce meta-operator dispatches the numeric bridge.
#
# `apply_reduction_op` is a pure function of two values, so an object operand
# used to fall through to a `0` default -- while `$a + $b`, `.reduce(&infix:<+>)`
# and `.reduce({$^a + $^b})` all got it right.
# --------------------------------------------------------------------------

class N { has $.n; method Numeric { $!n } }
my @c = N.new(n => 2), N.new(n => 3);

is @c.reduce(&infix:<+>), 5, '.reduce(&infix:<+>) over Numeric objects';
is @c.reduce({ $^a + $^b }), 5, '.reduce(block) over Numeric objects';
is ([+] @c), 5, '[+] over Numeric objects';
is ([*] @c), 6, '[*] over Numeric objects';
is ([-] @c), -1, '[-] over Numeric objects';
is ([+] @c).WHAT, Int, '[+] over Numeric objects returns an Int';
ok ([<] T.new(1/4), T.new(1/2)), '[<] over Real objects compares numerically';
is ([+] N.new(n => 2), 3), 5, '[+] mixing a Numeric object and a plain Int';

# A Match numifies through its matched text, in `[+]` as in `+$/`.
"2 + 3" ~~ / (\d+) \s* '+' \s* (\d+) /;
is ([+] $0, $1), 5, '[+] over Match captures';

# The same gap on the string side: the reduce table only reached `.gist`, so
# `[~]` rendered `S()S()` and `[lt]` compared those renderings, while the plain
# `~` / `lt` operators dispatch the operand's user `Str`.
class S { has $.s; method Str { $!s } }
my $sab = S.new(s => 'ab');
my $sc  = S.new(s => 'c');
is ($sab ~ $sc), 'abc', 'binary ~ dispatches a user Str';
is ([~] $sab, $sc), 'abc', '[~] dispatches a user Str';
ok ($sab lt $sc), 'binary lt dispatches a user Str';
ok ([lt] $sab, $sc), '[lt] dispatches a user Str';

# Plain values are untouched.
is ([+] 2, 3), 5, '[+] over plain Ints still works';
is ([~] "a", "b"), "ab", '[~] over Strs still works';
is ([~] 1, 2, 3), "123", '[~] over Ints still works';
is ([max] 2, 5, 3), 5, '[max] still works';

# --------------------------------------------------------------------------
# 3. `[∘]` over an empty operand list is the identity FUNCTION.
# --------------------------------------------------------------------------

my &composed = [∘];
ok &composed ~~ Callable, '[o] over an empty list is a Callable';
is composed("foo"), "foo", 'the [o] identity returns its argument';
is composed(42), 42, 'the [o] identity round-trips a non-Str too';

sub add1($n) { $n + 1 }
sub dbl($n)  { $n * 2 }
is ([∘] &add1, &dbl)(3), 7, '[o] over two routines still composes';
is ([∘] &add1)(3), 4, '[o] over one routine is that routine';

# --------------------------------------------------------------------------
# 4. The capitalised QuantHash spellings are COERCIONS: a positional Pair
#    argument is `key => weight`, exactly as for the `.Mix`/`.Bag` methods.
#    The lowercase `mix`/`bag`/`set` are `.new`-flavoured and keep Pairs opaque.
# --------------------------------------------------------------------------

# (compared key-by-key: the gist/raku element order of a QuantHash is not fixed)
for Mix(2 => 2, 4), MixHash(2 => 2, 4), Bag(2 => 2, 4), BagHash(2 => 2, 4) -> $q {
    is ($q.elems == 2 && $q{2} == 2 && $q{4} == 1), True,
        "{$q.^name}(2 => 2, 4) reads the Pair as key => weight";
}
my $nested = Bag(Bag(1, 1));
is ($nested.elems == 1 && $nested{1} == 2), True, 'Bag(...) spills a nested Bag';

# ... while the lowercase constructors keep every element opaque: the Pair
# itself is a key of weight 1, so there is no `2` key at all.
my $opaque = bag(2 => 2, 4);
is ($opaque.elems == 2 && $opaque{4} == 1 && $opaque{2} == 0), True,
    'bag(...) keeps a Pair opaque';

# The weighted set operators over MixHash operands then compute correctly.
my ($ma, $mb) = MixHash(2 => 2, 4), MixHash(2 => 1.5, 3 => 2, 4);
my $symdiff = $ma (^) $mb;
is $symdiff.elems, 2, 'MixHash (^) MixHash has two elements';
is $symdiff{2}, 0.5, 'MixHash (^) MixHash weight for 2';
is $symdiff{3}, 2, 'MixHash (^) MixHash weight for 3';

my $union = $ma (+) $mb;
is $union.elems, 3, 'MixHash (+) MixHash has three elements';
is $union{2}, 3.5, 'MixHash (+) MixHash weight for 2';
is $union{4}, 2, 'MixHash (+) MixHash weight for 4';

# --------------------------------------------------------------------------
# 5. `DateTime.julian-date` / `.modified-julian-date` are exact `Rat`s.
# --------------------------------------------------------------------------

my $dt = DateTime.new('2021-12-24T12:23:00.43Z');
is $dt.julian-date.WHAT, Rat, 'julian-date is a Rat';
is $dt.modified-julian-date.WHAT, Rat, 'modified-julian-date is a Rat';
is $dt.julian-date, 2459573.0159772, 'julian-date value is exact';
is $dt.modified-julian-date, 59572.5159772, 'modified-julian-date value is exact';
is $dt.julian-date - $dt.modified-julian-date, 2400000.5,
    'julian-date and modified-julian-date differ by exactly 2400000.5';

# --------------------------------------------------------------------------
# 6. A user sub named after a core type does not shadow the coercion call.
# --------------------------------------------------------------------------

sub Int(Str $s) { 'what?' }
is-deeply [Int, Int('42'), &Int('42')], [Int, 42, 'what?'],
    'a user sub Int does not occlude the Int(...) coercer';

# --------------------------------------------------------------------------
# 7. `MAIN` binds its command-line arguments through `val()`.
# --------------------------------------------------------------------------

# `val()` treats whitespace AROUND a number as insignificant, but a non-empty
# all-whitespace string is not numeric at all. Only the genuinely empty string
# numifies (to 0) -- this is what a `-y= ` MAIN option relies on.
is val(" 42 ").^name, 'IntStr', 'val() sees through whitespace around a number';
is val(" ").^name, 'Str', 'val(" ") is a plain Str';
is val("\t").^name, 'Str', 'val("\t") is a plain Str';
is val("").^name, 'IntStr', 'val("") is IntStr.new(0, "")';

# An allomorph's character-reading methods read its Str part, not its number.
is IntStr.new(0, "zero").ords.List, (122, 101, 114, 111),
    '.ords on an allomorph reads the Str part';
is IntStr.new(0, "").ords.elems, 0, '.ords on an empty-Str allomorph is empty';

my $prog = $*TMPDIR.add("numeric-coercion-main-{$*PID}.raku");
$prog.spurt: 'sub MAIN($pos, :$named) { say $pos.^name; say $named.^name; say $pos + 1 }';
my $proc = run($*EXECUTABLE, $prog, '--named=3.5', '42', :out, :err);
my $out = $proc.out.slurp(:close).chomp;
$proc.err.slurp(:close);
$prog.unlink;
is $out.lines[0], 'IntStr', 'a numeric MAIN positional arrives as an allomorph';
is $out.lines[1], 'RatStr', 'a numeric MAIN named value arrives as an allomorph';
is $out.lines[2], '43',     'the allomorph still works as a number';

done-testing;
