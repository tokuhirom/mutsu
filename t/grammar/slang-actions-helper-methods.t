use v6;
use lib 't/lib';
use Test;

# Which methods of a slang *actions* role are production overrides
# (ADR-13220). Rakudo calls an action only for the production that matched,
# looked up by name, so a method naming no production is a helper the actions
# call themselves: Test::Async::Decl's `add_phaser`, called as
# `self.add_phaser($/)` from its `package_declarator:sym<...>` actions. Slang
# activation must accept such a helper, and must still reject, by name, an
# override of a production mutsu does not support.

plan 4;

# t/lib/SlangDeclarator.rakumod's actions role carries the helper
# `gadget-ast` next to its two `package_declarator:sym<...>` handlers.
lives-ok { EVAL q[use SlangDeclarator; gadget HelperKnob { method spin { 'spun' } }; 1] },
    'an actions role with a helper method activates';

is EVAL(q[use SlangDeclarator; gadget HelperKnob2 { method spin { 'spun' } }; HelperKnob2.^name]),
    'HelperKnob2',
    'the declarators next to the helper still register';

# mutsu-specific: Rakudo implements every production, so it loads both of
# these; mutsu must refuse them rather than parse the rest of the unit under
# a grammar the slang meant to change (ADR-0026 §2.2).
throws-like { EVAL q[use SlangActionsUnknownProduction;] }, Exception,
    message => /'blockoid'/,
    'an unsupported override of a host production is still an error naming it';

throws-like { EVAL q[use SlangActionsUnknownCandidate;] }, Exception,
    message => /'statement_control:sym<frobnicate>'/,
    'an undeclared candidate of a known proto is treated as an override, not a helper';
