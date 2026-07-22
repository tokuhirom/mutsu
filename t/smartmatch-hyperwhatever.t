use v6;
use Test;

plan 11;

# A bare HyperWhatever pattern (`**`) on the RHS of `~~` matches ANY value,
# like `*` (Whatever). Regression: `$x ~~ (**)` returned False.
ok (1, 2, 4, 5, 6) ~~ (**), 'a non-empty list matches bare **';
ok ()              ~~ (**), 'the empty list matches bare **';
ok 5               ~~ (**), 'a scalar matches bare **';
ok "x"             ~~ (**), 'a string matches bare **';
ok Any             ~~ (**), 'a type object matches bare **';

# `**` inside a list pattern still constrains (this already worked — guard it).
ok (1, 3)          ~~ (1, **, 3), '** spans zero-or-more middle elements';
ok (1, 2, 4, 5, 3) ~~ (1, **, 3), '** spans several middle elements';
nok (1, 2, 4, 5, 6) ~~ (1, **, 5), 'trailing element must still match after **';
ok (1, 2, 3)       ~~ (**, 3), 'leading ** with a fixed tail';
nok (1, 2, 3)      ~~ (**, 5), 'leading ** but wrong tail fails';

# The full List.rakudoc example bundled as one check.
is-deeply [(1,2,3) ~~ (1,*,3), (1,2,3) ~~ (9,*,5), (1,3) ~~ (1,**,3), (1,2,4,5,6) ~~ (**)], [True, False, True, True], 'List.rakudoc smartmatch examples';
