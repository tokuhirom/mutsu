use v6;
use lib 't/lib';
use Test;

# A slang that adds a new *package declarator* (ADR-0091), the second half of
# the slang-activation story ADR-0026 started: `t/lib/SlangDeclarator.rakumod`
# mixes a grammar role into `$*LANG` from its `sub EXPORT`, and the role's
# `token package_declarator:sym<...>` candidates make `widget` and `gadget`
# package declarators for the rest of this compilation unit.
#
# `widget` builds a class with MetamodelX::WidgetHOW; `gadget` declares
# `$*PKGDECL := 'role'` and swaps the metaclass from inside the candidate.
# Those are the two shapes Test::Async's `test-hub` and `test-bundle` use.
# The whole fixture is valid Raku: it runs unchanged under rakudo, with
# byte-identical output.

use SlangDeclarator;

plan 11;

widget Thing {
    method hi { 'hello' }
}

is Thing.new.hi, 'hello', 'a class declared with a slang declarator works like any class';
is Thing.HOW.^name, 'MetamodelX::WidgetHOW',
    "the declarator's metaclass (from the EXPORT body's set_how) is attached";
is Thing.HOW.widget-tag(Thing), 'widget:Thing',
    'a method on the custom metaclass is reachable through .HOW';

gadget Knob {
    method spin { 'spun' }
}

is Knob.^name, 'Knob', 'a role declared with a slang declarator has its own name';
is Knob.HOW.^name, 'Perl6::Metamodel::ParametricRoleGroupHOW',
    "a declarator whose \$*PKGDECL is 'role' declares a role, not a class";

class UsesKnob does Knob { }
is UsesKnob.new.spin, 'spun', 'the role a slang declarator declares composes normally';

# The `unit` form: a whole compunit declared with the new keyword, which is how
# Test::Async spells its own bundles (`unit test-bundle Test::Async::Base;`).
use SlangUnitWidget;
use SlangUnitGadget;

is SlangUnitWidget.new.widget-name, 'SlangUnitWidget',
    '`unit <declarator> Name;` declares the whole compunit';
is SlangUnitWidget.HOW.^name, 'MetamodelX::WidgetHOW',
    'the unit form attaches the declarator metaclass too';

class UsesUnitGadget does SlangUnitGadget { }
is UsesUnitGadget.new.gadget-name, 'SlangUnitGadget',
    '`unit <role-declarator> Name;` declares a composable role';

# The keyword is lexical to the unit that `use`d the slang, exactly like the
# rule overrides in ADR-0026: an EVAL string is its own compilation unit and
# parses in the stock grammar, where `widget` is just an identifier.
dies-ok { EVAL q[widget EvalWidget { method x { 1 } }] },
    'the declarator keyword does not leak into an EVAL string';

# A keyword the slang did not register stays an ordinary undeclared name.
dies-ok { EVAL q[sprocket Nope { }] },
    'an unregistered declarator keyword is still a parse failure';
