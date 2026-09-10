use v6;
use Test;

# A negated character-class lookahead `<![...]>` must be a single negation.
# Regression: mutsu double-negated it (folding the `!` both into the class and
# into the Lookaround), inverting `<![...]>` into a positive lookahead.
# (raku-doc Language/regexes.rakudoc)

plan 10;

# <?[...]>  positive lookahead of the class
is ("3x" ~~ /<?[0..9]>\w/).Str, "3",   '<?[0..9]> matches before a digit';
nok ("ax" ~~ /<?[0..9]>\w/),           '<?[0..9]> fails before a non-digit';

# <![...]>  negative lookahead of the class (the fixed case)
is ("3x" ~~ /<![0..9]>\w/).Str, "x",   '<![0..9]> skips the digit, matches the letter';
is ("ax" ~~ /<![0..9]>\w/).Str, "a",   '<![0..9]> matches before a non-digit';

# <?-[...]>  positive lookahead of the *negated* class
is ("3x" ~~ /<?-[0..9]>\w/).Str, "x",  '<?-[0..9]> matches before a non-digit';
is ("ax" ~~ /<?-[0..9]>\w/).Str, "a",  '<?-[0..9]> matches before a non-digit (a)';

# <!-[...]>  negative lookahead of the negated class
is ("3x" ~~ /<!-[0..9]>\w/).Str, "3",  '<!-[0..9]> matches before a digit';
nok ("ax" ~~ /<!-[0..9]>\w/),          '<!-[0..9]> fails before a non-digit';

# The documented example that surfaced the bug.
my regex key {^^ <![#-]> \d+ }
is ("333" ~~ &key).Str, "333",         '<![#-]> after ^^ inside a named regex';
nok ("#333" ~~ &key),                  '<![#-]> rejects a leading # at line start';
