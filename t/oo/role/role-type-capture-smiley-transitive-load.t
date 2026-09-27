use v6;
use lib 't/lib';
use Test;
use RoleCaptureSmiley::Mid;

plan 7;

# A parametric role's `::T` capture used with a definedness smiley
# (`method m(T:U $class)`) must resolve to the composing class's type argument
# even when the role and the class that composes it are loaded only
# transitively. Composition recorded `T => Foo` for the role's methods but not
# the capture marker `T:U` resolution needs, so the parameter kept the literal
# constraint `Constraint:U` and rejected every argument. A bare `T $x` still
# worked. Reduced from MUGS::UI::CLI, whose every game UI died at load in
# `MUGS::UI.register-ui(..., self.WHAT)`.

is RoleCaptureSmiley::UI.register-ui(RoleCaptureSmiley::UI::Game), 'ui',
    'T:U accepts the exact type argument';
is RoleCaptureSmiley::UI.register-ui(RoleCaptureSmiley::Mid::Game), 'ui',
    'T:U accepts a subclass declared in another module';
is RoleCaptureSmiley::Client.register-implementation(RoleCaptureSmiley::Client::Game),
    'impl', 'a second role reusing the capture name binds its own type';
is RoleCaptureSmiley::Client.register-defined(RoleCaptureSmiley::Client::Game.new),
    'impl-defined', 'T:D accepts an instance of the type argument';

throws-like { RoleCaptureSmiley::UI.register-ui(RoleCaptureSmiley::Client::Game) },
    X::TypeCheck::Binding::Parameter,
    'T:U still rejects an unrelated type';
throws-like { RoleCaptureSmiley::Client.register-implementation(RoleCaptureSmiley::UI::Game) },
    X::TypeCheck::Binding::Parameter,
    'the other role is not bound to the first role\'s type';
dies-ok { RoleCaptureSmiley::UI.register-ui(RoleCaptureSmiley::UI::Game.new) },
    'T:U rejects an instance';
