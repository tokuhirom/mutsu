use v6;
use Test;

# Metamodel accessors that were missing or over-strict (PLAN 8.6 residue):
# `.^shortname` did not exist at all, and `.^ver`/`.^auth`/`.^api` threw
# X::Method::NotFound for builtin types, plain values, and undeclared
# modules/roles/enums where Rakudo answers v6.c / Mu / "".

plan 22;

# --- .^shortname (Metamodel::Naming) ---
class Foo { class Bar {} }
is Foo.^shortname, 'Foo', 'shortname of a top-level class is its name';
is Foo::Bar.^shortname, 'Bar', 'shortname drops the package qualifier';
is Int.^shortname, 'Int', 'shortname of a builtin type';
is 42.^shortname, 'Int', 'shortname on a plain value uses its type';

module M2 { class Deep::X {} }
is M2::Deep::X.^shortname, 'X', 'shortname drops every qualifier level';

role R3[::T] {}
is R3.^shortname, 'R3', 'shortname of a role';
is R3[M2::Deep::X].^shortname, 'R3[X]',
    'shortname shortens qualified names inside type args too';
is Set[Str].^shortname, 'Set[Str]', 'parameterized builtin keeps its args';

enum ShortE <se-a se-b>;
is ShortE.^shortname, 'ShortE', 'shortname of an enum';
subset ShortS of Int;
is ShortS.^shortname, 'ShortS', 'shortname of a subset';

# --- .^ver ---
# Core-setting language versions surface as plain Strs in Rakudo
# (Int.^ver.WHAT is Str); only a declared :ver adverb is a Version.
is-deeply Int.^ver, '6.c', 'builtin type .^ver is the Str "6.c"';
is-deeply 42.^ver, '6.c', '.^ver on a plain value is the Str "6.c"';
module UnverM { }
ok UnverM.^ver === Mu, 'unversioned module .^ver is Mu';
role UnverR { }
ok UnverR.^ver === Mu, 'unversioned role .^ver is Mu';
enum UnverE <ue-a ue-b>;
ok UnverE.^ver === Mu, 'unversioned enum .^ver is Mu';

# --- .^auth ---
class NoAuth { }
is NoAuth.^auth, '', 'class with no :auth has empty-string auth';
is 42.^auth, '', '.^auth on a plain value is empty string';
module NoAuthM { }
is NoAuthM.^auth, '', 'module with no :auth has empty-string auth';
class WithAuth:auth<github:me> { }
is WithAuth.^auth, 'github:me', 'declared :auth is still returned';

# --- .^api on a plain value ---
is 42.^api, '', '.^api on a plain value is empty string';

# --- .^isa returns the nqp boolean as an Int, matching Rakudo ---
is-deeply Foo.^isa(Any), 1, '.^isa answers with Int 1';
is-deeply Foo.^isa(ShortE), 0, '.^isa answers with Int 0';
