use v6;
use lib 't/lib';
use AttrNameTrait;
use Test;

plan 4;

# The META6 shape: a class declares `multi sub trait_mod:<is>` in its OWN body
# and applies it to attributes both in that body and inside NESTED classes.
# Two defects pinned here:
#  1. during a nested class's registration current_package is the nested name
#     ("M::Support"), and multi lookup probed only current_package + GLOBAL,
#     so the enclosing class body's candidates were invisible ("No matching
#     candidates" when a module also exported the proto, "unknown trait"
#     otherwise). The dispatch now walks up the package chain.
#  2. a named ARRAY param (`:@spec! (Opt $o, Version $v)`) matched a single
#     non-Positional value, out-dispatching the scalar candidate
#     (`Opt :$spec!`) and dying in the destructure binding.
# (Actually CALLING the array-form candidate with `[Optional, v1.2]` is a
# separate, still-open destructure-binding gap — see TODO_roast/BLOCKERS.md.)

enum Optionality <Mandatory Optional>;

class M {
    role MetaAttribute {
        has Optionality $.optionality is rw;
        has Version $.spec-version is rw = Version.new(0);
    }

    multi sub trait_mod:<is> (Attribute $a, Optionality :$specification!) {
        $a does MetaAttribute;
        $a.optionality = $specification;
    }

    multi sub trait_mod:<is> (Attribute $a, :@specification! (Optionality $optionality, Version $spec-version)) {
        $a does MetaAttribute;
        $a.optionality = $optionality;
        $a.spec-version = $spec-version;
    }

    class Support {
        has Str $.source is rw is specification(Optional);
    }

    has Str $.name is rw is specification(Mandatory);
}

is M.^attributes[0].optionality, Mandatory,
    'own-body attribute dispatches the enclosing-body trait multi';
is M::Support.^attributes[0].optionality, Optional,
    'nested-class attribute sees the enclosing class body trait multi';
is M.^attributes[0].spec-version, v0,
    'scalar form keeps the default spec-version';
ok M::Support.^attributes[0] ~~ M::MetaAttribute,
    'the mixed-in role smartmatches on the nested class attribute';
