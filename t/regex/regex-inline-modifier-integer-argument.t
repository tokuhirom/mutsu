use Test;

# An internal regex modifier with an integer argument switches its mode on
# unless the integer is zero, in either spelling (`:i(0)`, `:0i`), at the start
# of the pattern and inside it (#18713, roast S05-mass/rx.t).

plan 14;

is ~('abCdef' ~~ /:i(1) bcd/), 'bCd', ':i(1) ignores case';
is ~('abcdef' ~~ /:i(0) bcd/), 'bcd', ':i(0) matches exact case';
nok 'abCdef' ~~ /:i(0) bcd/, ':i(0) does not ignore case';
is ~('abCdef' ~~ /:1i bcd/), 'bCd', ':1i ignores case';
nok 'abCdef' ~~ /:0i bcd/, ':0i does not ignore case';
is ~('ABCDEF' ~~ /:2i ab [:999i cd ] ef/), 'ABCDEF', 'any non-zero count is on';
is ~('ABCDEF' ~~ /:1ignorecase ab [:ignorecase(1) cd ] ef/), 'ABCDEF',
    'the long name takes an argument too';
is ~('AbCdEf' ~~ /:i a [:i(0) b [:i(1) c [:0i d [:1i e [:i(0) f ] ] ] ] ]/), 'AbCdEf',
    'nested groups switch the mode lexically';
nok 'AbCdEF' ~~ /:i a [:i(0) b [:i(1) c [:0i d [:1i e [:i(0) f ] ] ] ] ]/,
    'the innermost :i(0) is case-sensitive';
nok 'abCdef' ~~ /x :i(0) bcd/, ':i(0) after an atom';
ok 'a b c def' ~~ /:s(1) b c [:s(0) d e f ]/, ':s(1) and :s(0)';
nok 'a b c def' ~~ /:s(0) b c [:s(1) d e f ]/, ':s(0) at the start';

# A negated adverb takes no argument: in `:!i(1)` the `(1)` is a capture.
nok 'aB' ~~ /:!i(1) aB/, ':!i(1): the (1) is the next atom';
is ~('1aB' ~~ /:!i(1) aB/), '1aB', ':!i(1) matches a literal 1 first';
