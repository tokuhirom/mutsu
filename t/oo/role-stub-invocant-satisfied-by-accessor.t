use Test;

# A role stub with an explicit invocant (`method r(::?ROLE:D:) { ... }`) takes
# no positional arguments, so a public attribute's accessor satisfies it just
# as it satisfies a plain `method r { ... }` stub (#9727). Data::Record's
# `Data::Record::Instance` role declares `method record(::?ROLE:D: --> T)`,
# which `Data::Record::Map` implements with `has %.record`.

plan 4;

role I { method r(::?ROLE:D:) { ... } }
class C does I { has $.r = 1 }
is C.new.r, 1, 'a scalar accessor satisfies a stub with an explicit invocant';

role J[::T] { method r(::?ROLE:D: --> T) { ... } }
class D does J[Map:D] { has %.r }
is-deeply D.new(r => {a => 1}).r, {a => 1}.Map.Hash,
    'a hash accessor satisfies a parametric stub with invocant and return type';

role U { method r(::?ROLE:U:) { ... } }
class E does U { has $.r = 2 }
is E.new.r, 2, 'an accessor satisfies a stub with a :U invocant too';

role P { method r { ... } }
class F does P { has $.r = 3 }
is F.new.r, 3, 'a stub without an explicit invocant still works';
