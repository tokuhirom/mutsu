use Test;

plan 11;

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
for { CORE::uc('x') }, 'CORE', { SETTING::lc('X') }, 'SETTING' -> &call, $pseudo {
    my $message = (try { call(); 'lived' }) // $!.message;
    ok $message ne 'lived' && !$message.contains("GLOBAL::$pseudo"),
        "a call through $pseudo:: dies without naming a GLOBAL::$pseudo package";
}
is &CORE::uc('x'), 'X', '&CORE::uc is still the core routine';

constant foo = 3;
throws-like { OUR::foo(1) }, X::Method::NotFound, message => /'CALL-ME'/,
    'a call through OUR:: invokes the stash value';
throws-like { UNIT::foo(1) }, X::Method::NotFound, message => /'CALL-ME'/,
    'a call through UNIT:: invokes the stash value';

# Type names keep resolving through the pseudo-package.
is CORE::Int.^name, 'Int', 'bare CORE::Type is the type object';
is CORE::Int:D.raku, 'Int:D', 'a type smiley still applies';
is CORE::Array[Int].^name, 'Array[Int]', 'a parameterization still applies';
is CORE::X::AdHoc.^name, 'X::AdHoc', 'a nested CORE:: name is still a type';
