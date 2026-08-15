use Test;

# `sub foo($a:) { }` (an invocant marker in a *sub*, not a method) and
# `sub bar($!x) { }` (an attribute-twigil parameter outside a method, which
# needs `self`) were already raising the right exception class, but two
# things kept roast's `~~ X::Comp` / `~~ X::Syntax` checks from seeing it:
#
# - X::Syntax::Signature::InvocantNotAllowed and X::Syntax::NoSelf were never
#   registered under X::Syntax at all, so a correctly-typed instance still
#   failed `~~ X::Comp` (roast/S06-signature/errors.t,
#   roast/S32-exceptions/misc2.t and others check this class hierarchy, not
#   just the leaf class).
# - Both exceptions' `.message` attribute leaked the "X::Type: " message-
#   convention prefix verbatim instead of just the description text.

plan 6;

try { EVAL 'sub foo($a:) { }' };
is $!.^name, 'X::Syntax::Signature::InvocantNotAllowed',
    'invocant marker in a sub signature is X::Syntax::Signature::InvocantNotAllowed';
is $!.message, 'Can only use the : invocant marker in the signature for a method',
    'the message matches rakudo exactly and does not repeat the class name';
ok $! ~~ X::Comp, 'X::Syntax::Signature::InvocantNotAllowed does X::Comp';

try { EVAL 'sub bar($!x) { }' };
is $!.^name, 'X::Syntax::NoSelf',
    'an attribute-twigil sub parameter is X::Syntax::NoSelf';
is $!.message, q{Variable $!x used where no 'self' is available},
    'the message does not repeat the class name';
ok $! ~~ X::Comp, 'X::Syntax::NoSelf does X::Comp';
