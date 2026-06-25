use Test;

plan 8;

# `.^ver` on a type with no declared version is `Mu` (an undefined type object),
# not an error. Rakudo returns Mu; mutsu previously threw X::Method::NotFound.
{
    class Unversioned { }
    nok Unversioned.^ver.defined, '.^ver of an unversioned class is undefined';
    ok Unversioned.^ver === Mu, '.^ver of an unversioned class is Mu';
}

# A literal `:ver<...>` is still returned as a Version.
{
    class Versioned:ver<1.2.3>:auth<me> { }
    is Versioned.^ver, v1.2.3, '.^ver returns the literal version';
    is Versioned.^auth, 'me', '.^auth returns the literal auth';
}

# `::?CLASS` resolves to the class inside an attribute default (it was `Any`).
{
    class WithName {
        has $.n = ::?CLASS.^name;
    }
    is WithName.new.n, 'WithName', '::?CLASS.^name in an attribute default';
}

# `::?CLASS.^ver` in an attribute default works for a literal version...
{
    class WithVer:ver<3.4.5> {
        has $.v = ::?CLASS.^ver;
    }
    is WithVer.new.v, v3.4.5, '::?CLASS.^ver in an attribute default (literal version)';
}

# ...and through a role-composed attribute default (the DBIish shape:
# DBDish::Driver has `$.Version = ::?CLASS.^ver`, composed into each driver).
{
    role Driver { has $.Version = ::?CLASS.^ver; }
    class Driver1:ver<9.9.9> does Driver { }
    is Driver1.new.Version, v9.9.9,
        '::?CLASS.^ver in a role-composed attribute default';
    # An unversioned consumer gets Mu, not an error.
    class Driver2 does Driver { }
    ok Driver2.new.Version === Mu,
        'role-composed ::?CLASS.^ver is Mu for an unversioned class';
}
