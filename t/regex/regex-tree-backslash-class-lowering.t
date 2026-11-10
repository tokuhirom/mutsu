use Test;

# Backslash classes lowered straight from the retained regex tree must match
# exactly what the string-parsed spelling matched.
plan 39;

ok "a_1" ~~ /^ \w+ $/, '\w+ matches word chars';
nok "a-1" ~~ /^ \w+ $/, '\w+ stops at a hyphen';
ok "-" ~~ /^ \W $/, '\W matches a non-word char';
nok "a" ~~ /^ \W $/, '\W rejects a word char';
ok " \t" ~~ /^ \s+ $/, '\s+ matches blanks';
ok "x" ~~ /^ \S $/, '\S matches non-space';
nok " " ~~ /^ \S $/, '\S rejects space';
nok "1" ~~ /^ \D $/, '\D rejects a digit';
ok "a" ~~ /^ \D $/, '\D matches a non-digit';
ok " \t" ~~ /^ \h+ $/, '\h+ matches horizontal space';
nok "\n" ~~ /^ \h $/, '\h rejects newline';
ok "x" ~~ /^ \H $/, '\H matches non-horizontal space';
ok "\n" ~~ /^ \v $/, '\v matches newline';
nok " " ~~ /^ \v $/, '\v rejects space';
ok " " ~~ /^ \V $/, '\V matches non-vertical space';
ok "\n" ~~ /^ \n $/, '\n matches newline';
ok "\r\n" ~~ /^ \n $/, '\n matches CRLF';
ok "a" ~~ /^ \N $/, '\N matches non-newline';
nok "\n" ~~ /^ \N $/, '\N rejects newline';
ok "\t" ~~ /^ \t $/, '\t matches tab';
nok " " ~~ /^ \t $/, '\t rejects space';
ok "a" ~~ /^ \T $/, '\T matches non-tab';
nok "\t" ~~ /^ \T $/, '\T rejects tab';
ok "\r" ~~ /^ \r $/, '\r matches CR';
ok "a" ~~ /^ \R $/, '\R matches non-CR';
ok "\x1B" ~~ /^ \e $/, '\e matches ESC';
ok "a" ~~ /^ \E $/, '\E matches non-ESC';
ok "\f" ~~ /^ \f $/, '\f matches form feed';
ok "a" ~~ /^ \F $/, '\F matches non-form-feed';

is ("ab 12 cd" ~~ /\w+ \s+ (\d+)/)[0].Str, '12', 'capture after \w+ \s+';
is ("a b" ~~ /\w \s \w/).Str, 'a b', 'mixed classes in a sequence';
is ("x\ty" ~~ /x \t y/).Str, "x\ty", 'tab between literals';
is ("é" ~~ /^ \w $/).Str, 'é', '\w is Unicode aware';
is ("\x2003" ~~ /\s/).Str, "\x2003", '\s matches Unicode space';
is ("aaa" ~~ /:i \w+/).Str, 'aaa', 'class under :i';

my token word { \w+ }
is ("hello world" ~~ /<word>/).Str, 'hello', 'class inside a token';
my regex sp { \s+ }
is ("a   b" ~~ /a <sp> b/).Str, 'a   b', 'class inside a named regex';
is "a1b2".comb(/\d/).join(","), '1,2', 'comb with \d';
is "a1b2".comb(/\D/).join(","), 'a,b', 'comb with \D';
