use Test;

# `:i` governs LITERAL comparison, not membership of a named character-property
# class: `<+upper>` still means "an uppercase character" under `:i`, not "any
# case variant of an uppercase character". mutsu used to widen the case-fold
# closure of the subject character BEFORE testing class membership, so under
# `:i` a lowercase letter satisfied `<upper>` (its uppercase fold does) and an
# uppercase letter satisfied `<lower>` -- for both a composite class
# (`<+upper -[A]>`) and a Unicode property inside one (`<+:Lu>`), as well as a
# bare named class (`<lower>`), which the parser also routes through the
# composite-class machinery. #8498, found deriving ADR-0099 Stage 1 first
# sets; not the prefilter's own bug, since it reproduces identically with
# `MUTSU_REGEX_PREFILTER=off` (both engine functions -- `composite_item_matches`
# and `class_matches_ignorecase` -- shared the same fold-before-test shape).
#
# An explicit literal or range item is unaffected: `:i` still folds those, as
# it always did (and rakudo's own fold makes a fold-equivalent character the
# FIRST match in a scan, so the isolated single-character subjects below keep
# that half of the coverage unambiguous).

plan 13;

# The issue's own repro: a composite class combining a named builtin with a
# subtraction.
is ~("zzz Q zzz" ~~ / :i <+upper -[A]> /), 'Q',
    'composite <+upper -[A]> under :i matches the actual uppercase letter';
nok ("zzz a zzz" ~~ / :i <+upper -[A]> /).defined,
    'composite <+upper -[A]> under :i does not widen to match a lowercase letter';

# A bare named builtin (parses through the same composite-class machinery).
is ~("ZZZ q ZZZ" ~~ / :i <lower> /), 'q',
    'bare <lower> under :i matches an actual lowercase letter';
nok ("ZZZ Q ZZZ" ~~ / :i <lower> /).defined,
    'bare <lower> under :i does not widen to match an uppercase letter';

# A Unicode property inside a composite class shares the same shape.
is ~("zzz Q zzz" ~~ / :i <+:Lu> /), 'Q',
    'composite <+:Lu> under :i matches an actual uppercase letter';
nok ("zzz a zzz" ~~ / :i <+:Lu> /).defined,
    'composite <+:Lu> under :i does not widen to match a lowercase letter';

# A negated named builtin inside a composite class is affected the same way,
# in the opposite direction: it still rejects a non-member correctly.
is ~("QQQ z QQQ" ~~ / :i <-lower> /), 'Q',
    'negated <-lower> under :i still matches a non-lowercase letter';

# A literal or an explicit range is unaffected -- `:i` still folds those.
is ~("Q" ~~ / :i q /), 'Q', 'a bare literal still folds under :i (upper subject)';
is ~("q" ~~ / :i Q /), 'q', 'a bare literal still folds under :i (lower subject)';
is ~("Q" ~~ / :i <[a..z]> /), 'Q',
    'an explicit range still folds under :i (upper subject, lower range)';
is ~("q" ~~ / :i <[A..Z]> /), 'q',
    'an explicit range still folds under :i (lower subject, upper range)';

# A class mixing a literal with a named builtin: the literal half still
# folds, the builtin half does not widen.
is ~("Q" ~~ / :i <+digit +[q]> /), 'Q',
    'a mixed class: the literal half still folds under :i';
nok ("Q" ~~ / :i <+digit +[!]> /).defined,
    'a mixed class: the builtin half does not widen to match an unrelated letter';

# vim: expandtab shiftwidth=4
