# `?` joins `^`/`~` in `duplicated_prefix_run`: a doubled `?` in *term*
# position is X::Syntax::DuplicatedPrefix, matching rakudo's "Expected a
# term, but found either infix ?? or redundant prefix ?".
#
# This was deferred (todo/tickets/duplicated-prefix-question-mark.md) because
# the naive fix regressed roast/S03-operators/ternary.t: `Z??`/`X??`/`R??`/`S??`
# (an attempted meta-op over the ternary) must stay X::Syntax::CannotMeta, not
# X::Syntax::DuplicatedPrefix. Landing `?` here was only safe once
# `cannot_meta_ternary_error` diagnosed those four adjacent spellings BEFORE a
# bare `Z`/`X` infix fallback ever let a lone `??` reach term position looking
# ambiguous — see news/2026-08/metaop-ternary-cannot-meta.md.
use Test;

plan 10;

# A doubled `?` in term position is X::Syntax::DuplicatedPrefix.
throws-like 'say ??1', X::Syntax::DuplicatedPrefix, prefixes => '??',
    'doubled ? in term position (say)';
throws-like 'my $x = ??1', X::Syntax::DuplicatedPrefix, prefixes => '??',
    'doubled ? in term position (initializer)';

# `???` is the warn-flavoured yada stub -- a real term -- so it must NOT be
# claimed as a duplicated prefix.
lives-ok { EVAL 'sub f { ??? }; 1' }, '??? (yada stub) is a real term, not a duplicated prefix';

# The regression this fix must not repeat: `Z??`/`X??`/`R??`/`S??` stay
# X::Syntax::CannotMeta, since the ternary cannot be metaopped at all.
for <Z X R S> -> $op {
    throws-like "1 $op?? 2 !! 3", X::Syntax::CannotMeta,
        :operator{.contains: '??'}, "$op?? stays CannotMeta, not DuplicatedPrefix";
}

# ...but a *spaced* meta letter is unaffected: `Z` completes as its own bare
# infix and the `??` that follows is an ordinary doubled prefix.
throws-like 'my $x = 1 Z ??2', X::Syntax::DuplicatedPrefix, prefixes => '??',
    'spaced Z leaves a following doubled ? as term position';

# ...and the single prefix, and the infix ternary, are unaffected.
is (?1), True, 'a single prefix ? still boolifies';
is (1 ?? 2 !! 3), 2, 'the infix ternary ?? !! is unaffected';
