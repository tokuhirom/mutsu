use Test;
use NativeCall;

plan 12;

# A parameterised role's body runs once per concrete parameterisation, at the
# point the parameterisation is first *instantiated* (composed into a class, or
# punned by calling a method on it). A body statement that dies therefore
# rejects that parameterisation, and the failure surfaces as
# X::Role::Instantiation wrapping the original exception.
#
# The shape that motivated this: `NativeHelpers::CStruct`'s `LinearArray[::T]`
# opens with `die "Need a CStruct" unless T.REPR eq 'CStruct'`. mutsu used to
# swallow the error while punning and silently accept the bad parameterisation.

class AStruct is repr('CStruct') {
    has uint64 $.a;
    has uint64 $.b;
}
class Ordinary { has $.x }

role Guarded[::T] {
    die "Need a CStruct" unless T.REPR eq 'CStruct';
    method describe() { "guarded:{T.^name}" }
}

# The accepting parameterisation composes and keeps working across repeats
# (composition is memoised, so the body must not re-run and must not re-throw).
is Guarded[AStruct].describe, 'guarded:AStruct', 'accepted parameterisation puns';
is Guarded[AStruct].describe, 'guarded:AStruct', 'and does so again';

# Naming a parameterisation does not instantiate it: the guard stays unrun.
my $lazy;
lives-ok { $lazy = Guarded[Ordinary] }, 'naming a parameterisation does not run the body';
is $lazy.^name, 'Guarded[Ordinary]', 'the unrun parameterisation still names itself';

# Punning it does instantiate it, and the guard rejects it.
dies-ok { Guarded[Ordinary].describe }, 'a method call on a rejected parameterisation dies';
dies-ok { Guarded[Ordinary].new }, '.new on a rejected parameterisation dies';

throws-like { Guarded[Ordinary].describe }, X::Role::Instantiation,
    'the failure is X::Role::Instantiation';

try Guarded[Ordinary].describe;
my $err = $!;
is $err.role.^name, 'Guarded', '.role is the role that could not be instantiated';
is $err.exception.^name, 'X::AdHoc', '.exception is the exception the body died with';
ok $err.message.contains("Could not instantiate role 'Guarded'"),
    'the message names the role';
ok $err.message.contains('Need a CStruct'), 'and quotes the original message';

# Composing the rejected parameterisation into a class fails the same way.
throws-like 'class Refused does Guarded[Ordinary] { }', X::Role::Instantiation,
    'composing a rejected parameterisation into a class dies too';
