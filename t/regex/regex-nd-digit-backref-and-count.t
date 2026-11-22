use Test;

# The numbers a regex takes — a positional backreference and a `**` count —
# may be written in any script's decimal digits (`$٠` is `$0`, `a**۳` is
# `a**3`), and a range takes its greed modifier with a colon too (`**:!`,
# `**:?`) (#18713, roast S05-mass/rx.t).

plan 7;

ok 'bookkeeper' ~~ /(.) $٠/, '$٠ is a backreference to $0';
is ~('bookkeeper' ~~ /(.) $٠/), 'oo', 'and matches what $0 matched';
is ~('aaaa' ~~ /a**۳/), 'aaa', 'an Nd-digit ** count';
nok 'aa' ~~ /a**۳/, 'which needs that many';
is ~('baaabbb' ~~ /a**:!2..4/), 'aaa', '**:! is greedy';
is ~('baaabbb' ~~ /a**:?2..4/), 'aa', '**:? is frugal';
is ~('xaaaay' ~~ /xa*:!y/), 'xaaaay', '*:! before a letter is the greed modifier';
