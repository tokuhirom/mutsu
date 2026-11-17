use Test;

plan 15;

throws-like {
    sub f { sub helper { 'h' }; MY::helper() }
    f()
}, X::Method::NotFound, message => /'CALL-ME'/,
    'a call through MY:: uses pseudo-stash lookup';

throws-like {
    sub f { sub helper { 'h' }; { OUTER::helper() } }
    f()
}, X::Method::NotFound, message => /'CALL-ME'/,
    'a call through OUTER:: uses pseudo-stash lookup';

# `CORE::uc(...)` looks up the sigilless `uc` (only `&uc` is in CORE), so it
# fails like any other pseudo-package call; `&CORE::uc` is the routine.
# (rakudo dies here too, with an internal "is_dispatcher on BOOTCode" X::AdHoc.)
dies-ok { CORE::uc('x') }, 'a call through CORE:: uses pseudo-stash lookup';
dies-ok { SETTING::lc('X') }, 'a call through SETTING:: uses pseudo-stash lookup';
is &CORE::uc('x'), 'X', '&CORE::uc is still the core routine';

# A bare `PSEUDO::name` term is the stash entry `PSEUDO::<name>`.
is-deeply CORE::uc, Nil, 'bare CORE::name is the sigilless stash key (absent: Nil)';
constant foo = 3;
{ is OUTER::foo, 3, 'bare OUTER::name reads an outer constant' }
is UNIT::foo, 3, 'bare UNIT::name reads a unit-scope constant';
is LEXICAL::foo, 3, 'bare LEXICAL::name reads a visible constant';
sub g { MY::foo }
is-deeply g(), Nil, 'bare MY::name does not see an outer scope';
throws-like { OUR::foo(1) }, X::Method::NotFound, message => /'CALL-ME'/,
    'a call through OUR:: invokes the stash value';

# Type names keep resolving through the pseudo-package.
is CORE::Int.^name, 'Int', 'bare CORE::Type is the type object';
is CORE::Int:D.raku, 'Int:D', 'a type smiley still applies';
is CORE::Array[Int].^name, 'Array[Int]', 'a parameterization still applies';
is CORE::X::AdHoc.^name, 'X::AdHoc', 'a nested CORE:: name is still a type';
