use Test;
use lib 't/lib';
use CompoundNameRole;

plan 12;

# A declaration written with a COMPOUND name installs its last component into
# the package named by the preceding components, and is itself qualified by the
# enclosing package - `module TT { my role Entry::Handler { } }` declares
# `TT::Entry::Handler`, exactly as `my class Entry::Handler` already did. The
# role path registered a compound name bare instead, so `TT::Entry::Handler`
# named nothing and `does TT::Entry::Handler` died as `X::InvalidType`. The
# scope declarator does not change this: `my`, `our` and none behave alike.

module TT {
    role Entry { }
    my role Entry::Handler { method handle-entry() { 'handled' } }
    my class Composer does TT::Entry::Handler { }
    my class Relative does Entry::Handler { }

    is TT::Entry::Handler.^name, 'TT::Entry::Handler', 'the qualified name resolves';
    is Entry::Handler.^name, 'TT::Entry::Handler', 'the relative name names the same role';
    ok Entry::Handler === TT::Entry::Handler, 'both spellings are one role, not two';
    is Composer.new.handle-entry, 'handled', 'a class composes it through the qualified name';
    is Relative.new.handle-entry, 'handled', 'and through the relative name';
    ok Composer ~~ TT::Entry::Handler, 'the composer does the role';
    is TT::Entry.WHO<Handler>.^name, 'TT::Entry::Handler',
        'Handler is installed into the Entry package';
}

module TU {
    class Entry { }
    our role Entry::Handler { method h() { 'our' } }
    is TU::Entry::Handler.^name, 'TU::Entry::Handler', 'an our-scoped compound name qualifies too';
}

module TV {
    class Entry { }
    role Entry::Handler { method h() { 'plain' } }
    is TV::Entry::Handler.^name, 'TV::Entry::Handler', 'and so does one with no scope declarator';
}

# The name must not leak into GLOBAL: the bare compound spelling is not a type
# of its own outside the declaring package.
nok (try ::('Entry::Handler').defined), 'the bare compound name is not global';

# The cross-file `unit module` form is what TAP.rakumod actually does, and the
# composing class lives in the module itself.
is make-state().handle-entry, 'handled', 'the same shape works across a file boundary';
is CompoundNameRole::Entry::Handler.^name, 'CompoundNameRole::Entry::Handler',
    'and the role is reachable through its package from the importer';
