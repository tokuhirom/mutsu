use Test;

# X::TypeCheck::Binding::Parameter.parameter is a Parameter object, not the
# parameter's name. Code that recovers from a binding failure introspects it --
# Cro::HTTP::Router decides 400 vs 401 vs 404 from `.named` and `.type`, and
# matches the parameter itself against the mixin type an `is auth` trait
# composed, none of which a bare Str can answer.

plan 7;

class Session { has $.admin }
subset Admin of Session where *.admin;

# Subset (constraint) failure.
{
    my &handler = -> Admin $user, $path { 'secret' };
    try handler(Session.new(:!admin), 'private');
    my $e = $!;
    isa-ok $e, X::TypeCheck::Binding::Parameter, 'subset failure is a binding error';
    isa-ok $e.parameter, Parameter, '.parameter is a Parameter object';
    is $e.parameter.name, '$user', '.parameter.name is the sigiled name';
    nok $e.parameter.named, '.parameter.named is False for a positional';
}

# Plain type failure.
{
    my &handler = -> Int $n { $n };
    try handler('nope');
    my $e = $!;
    isa-ok $e, X::TypeCheck::Binding::Parameter, 'type failure is a binding error';
    isa-ok $e.parameter, Parameter, '.parameter is a Parameter object';
    is $e.parameter.name, '$n', '.parameter.name is the sigiled name';
}
