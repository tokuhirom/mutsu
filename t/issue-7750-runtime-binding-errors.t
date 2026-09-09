use Test;

plan 14;

sub compact($message) { $message.subst(/\s+/, ' ', :g).trim }

# Runtime binding failures must keep their runtime exception and message. The
# "will never work" wrapper belongs only to a mismatch the compiler can prove
# from a statically visible call.
sub named-typed(Int :$i) { }
try { named-typed :i<forty-two> }
my $ex = $!;
isa-ok $ex, X::TypeCheck::Binding::Parameter,
    'typed named parameter keeps its runtime exception';
is compact($ex.Str),
    q{Type check failed in binding to parameter '$i'; expected Int but got Str ("forty-two")},
    'typed named parameter uses the runtime message';

sub constrained-slurpy(*@a where {$_.all ~~ Int}) { }
try { constrained-slurpy(<a>) }
$ex = $!;
isa-ok $ex, X::TypeCheck::Binding::Parameter,
    'where-constrained slurpy keeps its runtime exception';
is compact($ex.Str),
    q{Constraint type check failed in binding to parameter '@a'; expected anonymous constraint to be met but got Array (["a"])},
    'where-constrained slurpy uses the runtime message';

sub callback-signature(&c:(Int)) { }
sub callback-with-str(Str) { }
try { callback-signature(&callback-with-str) }
$ex = $!;
isa-ok $ex, X::TypeCheck::Binding::Parameter,
    'callable signature mismatch keeps its runtime exception';
is compact($ex.Str),
    q{Signature constraint check failed in binding to parameter '&c'; expected :(Int $) but got :(Str $)},
    'callable signature mismatch uses both signatures';

sub concrete-routine(Str:D $s, Int $limit) { }
try { concrete-routine Str, 3 }
$ex = $!;
isa-ok $ex, X::Parameter::InvalidConcreteness,
    'a routine concreteness failure keeps its exception type';
is compact($ex.Str),
    q{Parameter '$s' of routine 'concrete-routine' must be an object instance of type 'Str', not a type object of type 'Str'. Did you forget a '.new'?},
    'a routine concreteness failure names the routine and parameter';

class InvocantHints {
    method wants-type(::?CLASS:U:) { }
    method wants-instance(::?CLASS:D:) { }
}
try { InvocantHints.new.wants-type }
$ex = $!;
isa-ok $ex, X::Parameter::InvalidConcreteness,
    ':U invocant failure keeps its exception type';
is compact($ex.Str),
    q{Invocant of method 'wants-type' must be a type object of type 'InvocantHints', not an object instance of type 'InvocantHints'. Did you forget a 'multi'?},
    ':U invocant failure suggests multi';

try { InvocantHints.wants-instance }
$ex = $!;
isa-ok $ex, X::Parameter::InvalidConcreteness,
    ':D invocant failure keeps its exception type';
is compact($ex.Str),
    q{Invocant of method 'wants-instance' must be an object instance of type 'InvocantHints', not a type object of type 'InvocantHints'. Did you forget a '.new'?},
    ':D invocant failure suggests .new';

class FallbackSource does PositionalBindFailover {
    method iterator {
        class :: does Iterator {
            method pull-one { return 42 unless $++; IterationEnd }
        }.new
    }
}
sub first-five(@a) { @a[^5] }
is first-five(FallbackSource.new).raku,
    '(42, Nil, Nil, Nil, Nil)',
    'PositionalBindFailover coerces through its iterator during binding';
is first-five((1, 2).Seq).raku,
    '(1, 2, Nil, Nil, Nil)',
    'the built-in Seq failover also binds as a cached List';
