use Test;

# `#\`` not immediately followed by an opening bracket used to fall through to
# the generic "Confused." diagnosis (X::Syntax::Confused) instead of the
# specific X::Syntax::Comment::Embedded rakudo raises. Fixed by tagging the
# parser's "Opening bracket required for #` comment" message with the
# "X::Type: text" convention and registering the class under X::Syntax (it was
# entirely unregistered, so even a correctly-typed instance failed
# `~~ X::Comp`).
#
# roast/S32-exceptions/misc2.t and roast/S02-lexical-conventions/comments.t
# assert this under the real Test module (MUTSU_REAL_TEST=1).

plan 4;

try { EVAL '3 * #` (no closing bracket adjacency) 2' };
is $!.^name, 'X::Syntax::Comment::Embedded',
    'a #` not immediately followed by a bracket is X::Syntax::Comment::Embedded';
is $!.message, "Opening bracket required for #` comment",
    'the message matches rakudo exactly';
ok $! ~~ X::Comp, 'X::Syntax::Comment::Embedded does X::Comp (compile-time error)';
ok $! ~~ X::Syntax, 'X::Syntax::Comment::Embedded does X::Syntax';
