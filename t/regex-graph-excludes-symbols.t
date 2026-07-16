use Test;

# Raku's POSIX `<graph>` character class is Letters ∪ decimal digits (Nd) ∪
# Punctuation. Unlike the C/POSIX "any visible char", it EXCLUDES the Unicode
# Symbol categories (Sm/Sc/Sk/So), Marks, and non-decimal Number categories.
# mutsu previously matched every non-space/non-control char, so `<+graph -punct>`
# (a common "word char" idiom) wrongly kept symbols like `^`/`$`, breaking e.g.
# Template::Mustache's `token ident { <+ graph - punct> ... }`.

plan 6;

# Symbols (S*) are NOT graph.
ok !('^' ~~ /^<+graph>$/), "caret (Sk) is not <graph>";
ok !('\$' ~~ /^<+graph>$/), "dollar (Sc) is not <graph>";
ok !('+' ~~ /^<+graph>$/), "plus (Sm) is not <graph>";

# Letters, decimal digits, and P-punctuation ARE graph.
ok ('a' ~~ /^<+graph>$/) && ('1' ~~ /^<+graph>$/) && ('#' ~~ /^<+graph>$/),
    "letters, digits and P-punctuation are <graph>";

# The `<+graph -punct>` idiom yields alphanumerics only (no symbols, no punct;
# `_` is Connector Punctuation so it too is excluded).
my @word = < a Z 3 >;
my @nonword = ('^', '$', '~', '+', '=', '<', '>', '|', '#', '/', '_');
ok @word.all ~~ /^<+ graph - punct>$/, "alphanumerics pass <+graph -punct>";
ok @nonword.none ~~ /^<+ graph - punct>$/, "symbols and punct fail <+graph -punct>";
