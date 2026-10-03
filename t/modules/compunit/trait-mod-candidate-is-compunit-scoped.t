use v6;
use lib 't/lib';
use Test;

plan 8;

# A `multi trait_mod:<is>` is lexical to the compunit that declares it. A
# multi is hoisted to the start of its block, so a compunit's candidates exist
# before its own `use` statements run; mutsu used to let them take part in
# the trait dispatch of every module it loaded (#11310). Upstream
# `NativeCall.rakumod` is that shape: its `is symbol`/`is mangled`
# candidates captured `NativeCall::Types`' `is array_type(...)` and failed.

# This script's own candidate must not reach the modules loaded below either.
multi trait_mod:<is>(Mu:U $type, :$inner-mark!) {
    $type.^add_method('mark', my method mark { "script:$inner-mark" });
}

use TraitScopeOuter;

is TraitScopeOuter::inner-mark(), 'inner:1',
    'a used module dispatches its trait to its own candidate only';
is TraitScopeOuter::call-marked(), 'outer:1',
    'the loading module still dispatches to its own candidate';
is TraitScopeOuter::inner-array-type().^name, 'int32',
    'a core trait in the used module is not routed to a user candidate';
is TraitScopeOuter::kind-of(42), 'Int',
    'an exported my proto in the used module registers once';
is TraitScopeOuter::kind-of('x'), 'Mu', '... and dispatches across its multis';

class Mine is inner-mark(2) { }
is Mine.mark, 'script:2', 'the script dispatches to its own candidate';

sub traced() is outer-mark(3) { }
is traced(), 'outer:3', 'an imported candidate applies to the importer';

throws-like 'class Bogus is no-such-trait(1) { }', X::Inheritance::UnknownParent,
    'an is-trait no candidate accepts is an unknown parent, as without one';
