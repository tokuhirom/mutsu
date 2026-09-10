use v6;
use Test;

# `.^api` (the metamodel `api` accessor) reads a type's declared `:api(...)`.
# Previously mutsu (1) never stored the `:api` adverb (only `:ver`/`:auth`), and
# (2) `.^api` wasn't a recognized ClassHOW method, so `Foo.^api` threw
# `No such method 'api' for invocant of type 'Perl6::Metamodel::ClassHOW'`.
# zef's `Zef::Pluggable !try-load` reads `Zef.^api`.

plan 7;

module ModWithApi:api<3> { }
is ~ModWithApi.^api, '3', 'module :api<3> is readable via .^api';

class ClassWithApi:api<7> { }
is ~ClassWithApi.^api, '7', 'class :api<7> is readable via .^api';

# A type with no declared :api has an empty-string api (matches Rakudo).
class NoApi { }
is ClassWithApi.^api ne '', True, 'declared api is non-empty';
is NoApi.^api, '', 'class with no :api has empty-string api';

module NoApiMod { }
is NoApiMod.^api, '', 'module with no :api has empty-string api';

# .^ver and .^auth still work alongside :api on the same declaration.
module Full:ver<1.2.3>:auth<github:me>:api<5> { }
is ~Full.^ver, '1.2.3', 'ver still works with api present';
is ~Full.^api, '5', 'api works with ver/auth present';
