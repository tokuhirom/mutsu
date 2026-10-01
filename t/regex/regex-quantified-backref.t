# A quantifier after a backreference (`$0*`, `$<x>+`, `$0 ** 0..3`, `$0?`)
# applies to the backreference. The regex parser used to push the backreference
# and move on, so the quantifier was never read and every such pattern failed
# to match. Values verified against rakudo. A backreference that can match
# empty (`(a?) $0*`) is also a loop body the compiled regex engine used to
# decline (`nullable-loop`, ADR-0135); it now compiles, each iteration
# committed as the walk's chain commits it.
use Test;

plan 12;

is ~("aab" ~~ /(a) $0* b/), 'aab', '$0*';
is ~("aab" ~~ /(a) $0+ b/), 'aab', '$0+';
is ~("aab" ~~ /(a) $0+? b/), 'aab', '$0+? grows until the rest matches';
is ~("ab" ~~ /(a) $0? b/), 'ab', '$0? takes zero';
is ~("aab" ~~ /(a) $0 ** 0..3 b/), 'aab', '$0 ** 0..3';
is ~("abab" ~~ /(ab) $0 ** 1/), 'abab', '$0 ** 1';
is ~("xyxyxyz" ~~ /(xy) $0+ z/), 'xyxyxyz', 'a multi-character capture repeated';
is ~("aab" ~~ /$<x>=(a) $<x>* b/), 'aab', 'a named backreference repeated';
is ~("aa-aa" ~~ /(a+) "-" $0? $/), 'aa-aa', 'an optional backreference before an anchor';
is ~("aaab" ~~ /(a?) $0* b/), 'aaab', 'a nullable capture repeated';
is ~("aaa" ~~ /(a) $0 $0/), 'aaa', 'unquantified backreferences are unchanged';
is ~("aa" ~~ /$0=(a) $0/), 'aa', 'a numbered alias is not a backreference';
