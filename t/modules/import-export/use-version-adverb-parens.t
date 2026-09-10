use v6;
use Test;

plan 4;

# A `use` statement may carry version/authority/api selector adverbs. These are
# NOT import tags — they refine which distribution to load. Both the angle-bracket
# literal form (`:ver<1.0>`) and the parenthesized-expression form
# (`:ver($expr)`, `:auth(Zef.^auth)`) must be consumed and discarded rather than
# treated as import tags. Regression: zef modules open with
#   use Zef:ver($?DISTRIBUTION.meta<version>):api(...):auth(...);
# which previously died with `no such tag 'ver'/'auth'`.

# 1. Angle-bracket selector form still parses and loads.
lives-ok { EVAL 'use Test:ver<1.0>; my $x = 42; $x.so' }, ':ver<...> angle-bracket selector';

# 2. Parenthesized selector form parses and loads (the zef idiom).
lives-ok { EVAL 'use Test:ver(1.0):api(1):auth(q{github:test}); my $x = 42; $x.so' },
    ':ver(...):api(...):auth(...) parenthesized selectors';

# 3. A parenthesized selector with a complex expression value.
lives-ok { EVAL 'use Test:ver("6.c" // "*"):auth("github:" ~ "x"); my $x = 42; $x.so' },
    'parenthesized selector with expression value';

# 4. Selector adverbs do not consume a following real import argument.
lives-ok { EVAL 'use Test:ver(1.0) <ok>; my $x = 42; $x.so' },
    'selector adverb followed by import list';
