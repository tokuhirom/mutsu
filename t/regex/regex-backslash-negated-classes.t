use Test;

# The upper case of a backslash class letter is its negation: `\R` is
# "not a carriage return" (not Perl 5's any-newline) and `\E` is "not ESC",
# both as atoms and inside a character class (#11444).

plan 13;

ok  "x"  ~~ /\E/,     '\E matches a non-ESC character';
nok "\e" ~~ /^\E$/,   '\E does not match ESC';
ok  "x"  ~~ /\R/,     '\R matches a non-CR character';
nok "\r" ~~ /^\R$/,   '\R does not match a carriage return';
ok  "\n" ~~ /^\R$/,   '\R matches a line feed';
is ~("a\rb" ~~ /\R+/), 'a', '\R+ stops at the carriage return';

ok  "x"  ~~ /<[\R]>/,    '<[\R]> matches a non-CR character';
nok "\r" ~~ /^<[\R]>$/,  '<[\R]> does not match a carriage return';
ok  "x"  ~~ /<[\E]>/,    '<[\E]> matches a non-ESC character';
nok "\e" ~~ /^<[\E]>$/,  '<[\E]> does not match ESC';
nok "\t" ~~ /^<[\T]>$/,  '<[\T]> does not match a tab';
nok "\f" ~~ /^<[\F]>$/,  '<[\F]> does not match a form feed';
ok  "a"  ~~ /^<[\F]>$/,  '<[\F]> matches a non-form-feed character';
