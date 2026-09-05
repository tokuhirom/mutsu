use Test;

# A bracketing quote whose delimiter is REPEATED (`Q[[ ... ]]`, `q{{ ... }}`,
# `Q<<< ... >>>`) uses the whole repeated run as one delimiter, and nesting is
# counted in units of that run -- not of the single bracket. So `Q[[1]]` is the
# doubled-delimiter quote of `1`, NOT a single-bracket quote of `[1]`, and
# `Q[[a[b]]]` is a syntax error because the lone `[` inside never balances a
# `[[`. Every expectation below was taken from rakudo.
#
# This file exists because a bug report claimed `Q[[1]]` should yield `[1]`.
# It should not -- mutsu already agreed with rakudo -- so pin the whole family
# so the (correct) reading cannot be "fixed" into a regression.

plan 25;

# --- doubled delimiter, each bracket pair ------------------------------------
is Q[[1]], '1', 'Q[[...]] is a doubled delimiter, not a nested single one';
is q[[1]], '1', 'q[[...]] likewise';
is qq[[1]], '1', 'qq[[...]] likewise';
is Q{{1}}, '1', 'Q{{...}} likewise';
is Q<<1>>, '1', 'Q<<...>> likewise';
is Q<<<1>>>, '1', 'a tripled delimiter works the same way';
is Q[[[1]]], '1', 'a tripled bracket delimiter works the same way';

# --- content that merely *contains* brackets ---------------------------------
is Q[[]], '', 'an empty doubled-delimiter quote is the empty string';
is Q[[ 1 ]], ' 1 ', 'surrounding space is content';
is Q[[a]], 'a', 'plain content';
is Q[[a[b]c]], 'a[b]c', 'a balanced single pair inside is content';
is Q[[a] [b]], 'a] [b', 'an unbalanced single bracket inside is content';
is Q[[1] [2]], '1] [2', 'the close is the first unnested `]]`';
is Q[[a[[b]]c]], 'a[[b]]c', 'a nested DOUBLED pair is counted and kept';
is Q<<a>b>>, 'a>b', 'an unbalanced single `>` inside is content';
is Q<<a<<b>>c>>, 'a<<b>>c', 'a nested doubled angle pair is counted and kept';

# --- single delimiter is unaffected -----------------------------------------
is Q[x[1]], 'x[1]', 'a single delimiter still counts single-bracket nesting';
is Q[a[b]c], 'a[b]c', 'nesting inside a single delimiter';
is Q[ [1] ], ' [1] ', 'a leading nested pair inside a single delimiter';

# --- the quote is an ordinary term -------------------------------------------
is Q[[1]] ~ 'x', '1x', 'a doubled-delimiter quote composes like any term';

# --- qq interpolates inside a doubled delimiter ------------------------------
my @a = 1, 2;
my %h = a => 1;
is qq[[x@a[0]y]], 'x1y', 'qq interpolates an indexed array inside `[[ ]]`';
is qq[[%h<a>]], '1', 'qq interpolates a hash subscript inside `[[ ]]`';
is qq{{@a[0]}}, '1', 'qq interpolates inside `{{ }}`';

# --- an unbalanced doubled delimiter is a syntax error -----------------------
# `[[1] 2]` never closes: the `]]` run required by the `[[` starter is absent.
throws-like { EVAL 'Q[[1] 2]' }, Exception,
    'an unclosed doubled delimiter is a syntax error';
# A lone `[` cannot be balanced by the `]]` close either.
throws-like { EVAL 'Q[[a[b]]]' }, Exception,
    'a single bracket does not balance a doubled delimiter';

done-testing;
