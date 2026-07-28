use v6;
use Test;

# Punning a *parameterised* role (`R[Int].new`) used to build an ad-hoc mixin
# instance: it never ran the role's `BUILD`, never evaluated the role body's
# deferred statements against the type parameter, and reported the bare role
# name. It now goes through the same composition path as `class C does R[Int]`,
# which is what binds the type parameters and pulls in `BUILD`.

plan 17;

role Built[::T] {
    has $.made is rw = 'unset';
    submethod BUILD() { $!made = 'by BUILD' }
    method elem-type() { T }
}

is Built[Int].new.made, 'by BUILD', 'parameterised role pun runs BUILD';
is Built[Int].new.elem-type.^name, 'Int', 'type parameter reaches methods';
is Built[Int].new.^name, 'Built[Int]', 'pun reports the parameterised name';
is Built[Int].WHAT.gist, '(Built[Int])', 'pun type object gists parameterised';

# A `my` in the role body is initialised per composition, from the bound type
# parameter — the body statements are deferred until the parameters exist.
role Sized[::T] {
    my $name = T.^name;
    my $len = T.^name.chars;
    method name() { $name }
    method len() { $len }
}

is Sized[Int].new.name, 'Int', 'role-body my sees the type parameter';
is Sized[Int].new.len, 3, 'role-body my computes from the type parameter';
is Sized[Rat].new.name, 'Rat', 'a second composition re-runs the body';

# The role body of a *plain* role still runs once, at declaration.
{
    my $runs = 0;
    role Plain { }
    is Plain.new.^name, 'Plain', 'plain role pun keeps the bare name';
    $runs = 1;
    is $runs, 1, 'plain role pun is unaffected';
}

# Smart-matching an instance against the parameterisation. The instance carries
# no type-argument markers; which parameterisation it satisfies is recorded on
# the class that composed the role.
role Holder[::T] { has $.v; }
class Wrapped does Holder[Int] { }

ok Holder[Int].new ~~ Holder, 'pun matches the base role';
ok Holder[Int].new ~~ Holder[Int], 'pun matches its own parameterisation';
nok Holder[Int].new ~~ Holder[Str], 'pun rejects a different parameterisation';
ok Wrapped.new ~~ Holder[Int], 'class instance matches the composed parameterisation';
nok Wrapped.new ~~ Holder[Str], 'class instance rejects a different parameterisation';

# A value (non-type) role parameter is compared by its spelling.
role Tagged[$n] { method tag() { $n } }
ok Tagged[42].new ~~ Tagged[42], 'value-parameterised pun matches its parameterisation';

# A class composing a role that itself does a *built-in* role satisfies that
# built-in role too (`Real` is not in the user role registry).
role Measured does Real { has $.v = 1; method Bridge() { $!v } }
role MeasuredT[::T] does Real { has $.v = 1; method Bridge() { $!v } }
class Ruler does Measured { }

ok Ruler.new ~~ Real, 'class composing a role that does Real is Real';
ok MeasuredT[Int].new ~~ Real, 'parameterised pun of such a role is Real too';
