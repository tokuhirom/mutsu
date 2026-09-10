use Test;

# The positional-light call path classifies each parameter's type constraint
# (and the declared return type) once at registration time -- `FastParamCheck`
# on the `CompiledFunction` -- so the per-call check dispatches on the *value*
# once and answers from a tag, instead of matching the constraint spelling on
# every call. This pins that the tagged check agrees with the by-name one it
# replaced across every shape it distinguishes: the five concrete types, the
# `Any`/`Mu`/`Cool` wildcards, bare type objects, allomorphs (mixins), and the
# return-type asymmetry (a returned type object must match the declared return
# type by name even when that type is a wildcard).

plan 23;

sub takes-int(Int $n --> Int) { $n }
is takes-int(7), 7, 'Int parameter accepts an Int';
is takes-int(2 ** 70), 1180591620717411303424, 'Int parameter accepts a BigInt';
is takes-int(Int).^name, 'Int', 'Int parameter accepts the bare Int type object';

sub takes-str(Str $s --> Str) { $s }
is takes-str("hi"), "hi", 'Str parameter accepts a Str';
is takes-str(<42>), "42", 'Str parameter accepts an IntStr allomorph';

sub takes-num(Num $x --> Num) { $x }
is takes-num(1e0), 1e0, 'Num parameter accepts a Num';

sub takes-bool(Bool $x --> Bool) { $x }
is takes-bool(True), True, 'Bool parameter accepts a Bool';

sub takes-rat(Rat $x --> Rat) { $x }
is takes-rat(1/2), 0.5, 'Rat parameter accepts a Rat';

sub takes-cool(Cool $c) { $c.^name }
is takes-cool(1), 'Int', 'Cool parameter accepts an Int';
is takes-cool("s"), 'Str', 'Cool parameter accepts a Str';
is takes-cool(Int), 'Int', 'Cool parameter accepts a bare type object';

class Widget {}
sub takes-any(Any $a) { $a.^name }
is takes-any(Widget.new), 'Widget', 'Any parameter accepts a user instance';
is takes-any(Widget), 'Widget', 'Any parameter accepts a user type object';

sub takes-mu(Mu $a) { $a.^name }
is takes-mu(1), 'Int', 'Mu parameter accepts an Int';

sub allomorph-int(Int $n) { $n + 1 }
is allomorph-int(<42>), 43, 'Int parameter accepts an IntStr allomorph';

# A bare type object satisfies the declared *return* type only when it names
# that very type -- unlike a parameter, where `Any`/`Mu`/`Cool` accept anything.
sub ret-type-object($x --> Int) { Int }
is ret-type-object(1).^name, 'Int', 'a returned type object matching the return type passes';

sub ret-any($x --> Any) { Widget.new }
is ret-any(1).^name, 'Widget', 'an instance satisfies an Any return type';

sub ret-nil($x --> Int) { Nil }
is ret-nil(1).defined, False, 'Nil passes any return type';

# The mismatching cases still fail, and the routine keeps working afterwards.
sub bad-param(Int $n) { $n }
my $str-arg = "x";
dies-ok { bad-param($str-arg) }, 'a Str argument fails an Int parameter';
is bad-param(3), 3, 'the routine still binds correctly after a failed call';

sub bad-return($x --> Int) { "s" }
dies-ok { bad-return(1) }, 'a Str return value fails an Int return type';

sub bad-wide(Cool $c) { $c }
dies-ok { bad-wide(Widget.new) }, 'a user instance fails a Cool parameter';

# The tag is per parameter, so a mixed signature checks each one on its own.
sub mixed(Int $i, Str $s, $untyped) { "$i|$s|" ~ $untyped.^name }
is mixed(1, "a", Widget), '1|a|Widget', 'each parameter is checked against its own constraint';
