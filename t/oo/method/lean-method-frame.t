use Test;

# The lean method frame (#18267, ADR-18244 stage D): a repeatedly called
# method with plain positional `$` parameters and none of the optional frame
# machinery runs in a short frame decided once per method-call plan. Each
# case below is called in a loop so the plan is installed and replayed, and
# checks one thing the general binder does that the lean frame must keep.

plan 27;

class C {
    has $.a = 1;
    has $!k;
    has @.log;
    method m0() { 42 }
    method m1($x) { $x + 1 }
    method m2($x, $y) { $x ~ $y }
    method setk($v) { $!k = $v }
    method getk() { $!k }
    method push-log($v) { @!log.push($v); self }
    method me() { self }
    method early($x) { return 'neg' if $x < 0; 'pos' }
    method failing($x) { fail "bad $x" if $x > 2; $x }
    method dying($x) { die "died $x" if $x == 3; $x }
    method ro($x) { $x = 5 }
    method cls() { $?CLASS.^name }
    method routine-name() { &?ROUTINE.name }
    method callee($x) { self.m1($x) * 2 }
    method rec($n) { $n <= 0 ?? 0 !! $n + self.rec($n - 1) }
    method topic($x) { $_ }
    method dyn() { $*LEAN-DYN }
    method typed(Int $x) { $x * 2 }
    method defined(Str:D $s) { $s.chars }
    method anything(Mu $m) { $m.^name }
}

my $o = C.new;
my @r;

@r = (^5).map: { $o.m0 };
is-deeply @r, [42 xx 5], 'zero-argument method';

@r = (^5).map: { $o.m1($_) };
is-deeply @r, [1, 2, 3, 4, 5], 'one positional argument';

@r = (^5).map: { $o.m2($_, 'x') };
is-deeply @r, ['0x', '1x', '2x', '3x', '4x'], 'two positional arguments';

$o.setk($_) for ^5;
is $o.getk, 4, 'an attribute assignment in the body reaches the instance';

$o.push-log($_) for ^5;
is-deeply $o.log, [0, 1, 2, 3, 4], 'an attribute aggregate mutated in the body';

my $same = True;
$same &&= $o.me === $o for ^5;
ok $same, 'returning self returns the invocant';

@r = (-2..2).map: { $o.early($_) };
is-deeply @r, ['neg', 'neg', 'pos', 'pos', 'pos'], 'an explicit return';

my @f = (^5).map: { $o.failing($_) };
ok @f[4] ~~ Failure, 'fail makes a Failure result';
is @f[2], 2, 'a non-failing call returns its value';
@f[3].so; @f[4].so;    # handle the Failures

my $died = 0;
for ^5 { try { $o.dying($_); CATCH { default { $died++ } } } }
is $died, 1, 'an exception escapes the frame';

my $ro-dies = 0;
for ^5 { try { $o.ro($_); CATCH { default { $ro-dies++ } } } }
is $ro-dies, 5, 'a parameter is read-only';

@r = (^5).map: { $o.cls };
is-deeply @r, ['C' xx 5], '$?CLASS';

@r = (^5).map: { $o.routine-name };
is-deeply @r, ['routine-name' xx 5], '&?ROUTINE';

@r = (^5).map: { $o.callee($_) };
is-deeply @r, [2, 4, 6, 8, 10], 'a method calling a method';

@r = (^5).map: { $o.rec($_) };
is-deeply @r, [0, 1, 3, 6, 10], 'recursion';

@r = (^3).map: { $o.topic($_) };
is-deeply @r.map(*.^name).List, ('Any', 'Any', 'Any'), 'a method has its own $_';

my $*LEAN-DYN = 'outer';
@r = (^3).map: { $o.dyn };
is-deeply @r, ['outer' xx 3], 'a dynamic variable of the caller';

my $mu-err = 0;
for ^5 { try { $o.m1($_ == 4 ?? Mu !! $_); CATCH { default { $mu-err++ } } } }
is $mu-err, 1, 'an untyped parameter refuses Mu';

# A caller's variable of the parameter's name is untouched.
my $x = 'caller';
$o.m1($_) for ^5;
is $x, 'caller', 'a parameter does not leak into the caller';

# The plan is replaced when the class changes.
class D { method v($x) { "one $x" } }
my $d = D.new;
my @v = (^3).map: { $d.v($_) };
D.^add_method('w', method ($x) { "w $x" });
D.^compose;
@v.append: (^2).map: { $d.v($_) };
is-deeply @v, ['one 0', 'one 1', 'one 2', 'one 0', 'one 1'], 'calls after a registry write';

# A subclass call resolves the inherited method with the receiver's class.
class E is C { method m1($x) { $x * 10 } }
my $e = E.new;
@r = (^3).map: { $e.m1($_) };
is-deeply @r, [0, 10, 20], 'an override in a subclass';
@r = (^3).map: { $e.m0 };
is-deeply @r, [42, 42, 42], 'an inherited method';

# Typed parameters: the check runs per call; a refusal reports the general
# binder's error.
@r = (^5).map: { $o.typed($_) };
is-deeply @r, [0, 2, 4, 6, 8], 'a typed parameter';
my $typed-err = 0;
for ^5 { try { $o.typed($_ == 4 ?? 'four' !! $_); CATCH { default { $typed-err++ } } } }
is $typed-err, 1, 'a typed parameter refuses another type';
@r = (^3).map: { $o.defined('ab' x $_) };
is-deeply @r, [0, 2, 4], 'a :D parameter';
throws-like { $o.defined(Str) }, X::Parameter::InvalidConcreteness, 'a :D parameter refuses a type object';
@r = (Mu, 1, 'a').map: { $o.anything($_) };
is-deeply @r, ['Mu', 'Int', 'Str'], 'a Mu parameter takes anything';
