unit module Matcher;

# Two candidates so the parser must know the arity/type is a positional call,
# not just a bareword -- a single-candidate proto is not enough to reproduce
# the bug this fixture guards against.
multi matches(Int $n, Str:D $s) is export { say "str: $n $s" }
multi matches(Int $n, Buf:D $b) is export { say "buf: $n" }
