use Test;

# A parse error that the parser diagnosed precisely spells its Raku exception
# class in the `"X::Type: text"` message convention. Such a diagnosis used to be
# flattened twice on the way out -- the statement-list loop stringified it into
# "expected statement at line N: ..." and `parse_program` wrapped that in
# "Confused. parse error at ...: expected A or B or ..." -- so the class was
# buried in a sentence and the exception arrived as the catch-all
# `X::Syntax::Confused`. Both layers now propagate the classified alternative.
#
# roast/S03-metaops/{cross,not,zip}.t and S03-operators/is-divisible-by.t all
# assert these classes through `throws-like`.

plan 6;

sub class-of($code) {
    try { EVAL $code };
    $!.^name;
}

is class-of('3 X. list'), 'X::Syntax::CannotMeta',
    'a dotty infix under a metaoperator is X::Syntax::CannotMeta';
is class-of('1 !% 2'), 'X::Syntax::CannotMeta',
    'negating a non-iffy infix is X::Syntax::CannotMeta';
is class-of('1 !+ 2'), 'X::Syntax::CannotMeta',
    'negating + is X::Syntax::CannotMeta';
is class-of('3 X. "foo"'), 'X::Obsolete',
    'the Perl 5 concat operator is X::Obsolete, not Confused';

# The classified message must not lose its own text, and a failure the parser
# genuinely cannot describe still falls back to X::Syntax::Confused.
try { EVAL '1 !% 2' };
ok $!.message.contains('not iffy enough'), 'the diagnosis text survives';

is class-of('1 1'), 'X::Syntax::Confused',
    'an undiagnosed parse failure is still X::Syntax::Confused';
