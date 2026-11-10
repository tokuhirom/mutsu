use v6.*;
use Test;

# Under `use v6.*`, `CORE::{"v6c"}` is the package of the 6.c core types and
# `.WHO{$name}` finds a core type by name (Rakudo::Type::Introspection's
# `core()` relies on it); `CORE::` itself lists the core types.
plan 9;

is (try CORE::{"v6c"}).^name, 'CORE::v6c', 'CORE::{"v6c"} is the revision package';
is (try CORE::{"v6c"}.WHO{"Int"}).raku, 'Int', 'a core type by name';
is (try CORE::{"v6e"}.WHO{"PseudoStash"}).raku, 'PseudoStash', 'also under v6e';
ok !(try CORE::{"v6c"}.WHO{"NoSuchCoreType"}).defined, "an unknown name is not a type";

ok CORE::.keys.grep('Cool'), 'CORE:: lists Cool';
ok CORE::.keys.grep('Mu'), 'CORE:: lists Mu';
is CORE::<Cool>.raku, 'Cool', 'CORE::<Cool> is the type object';

sub stash-name(CORE::v6c::PseudoStash:D $stash = CORE::) { $stash.^name }
is stash-name(), 'PseudoStash', 'a CORE::v6c:: type constraint with a CORE:: default';
sub bound(CORE::v6c::Int:D $n) { $n }
throws-like { bound("x") }, X::TypeCheck::Binding::Parameter, 'and it still rejects a non-member';
