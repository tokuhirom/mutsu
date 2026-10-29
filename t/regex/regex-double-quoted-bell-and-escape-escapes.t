use Test;

# `\e` (ESC) and `\a` (BEL) inside a double-quoted regex literal
# (Selkie t/82-app-facade: a tmux passthrough sequence matched with
# /"\ePtmux;" .* "\e\e]0;Hi\a" .* "\e\\"/).

plan 8;

my $s = "\ePtmux;\e\e]0;Hi\a\e\\";
ok $s ~~ /"\ePtmux;" .* "\e\e]0;Hi\a" .* "\e\\"/, 'the whole tmux title sequence matches';
ok "x\e" ~~ /. "\e"/, '"\e" after an atom is ESC';
ok "x\e" ~~ /.* "\e"/, '"\e" after .* is ESC';
nok "xe" ~~ /.* "\e"/, '"\e" is not the letter e';
ok "x\a" ~~ /.* "\a"/, '"\a" after .* is BEL';
nok "xa" ~~ /.* "\a"/, '"\a" is not the letter a';
nok "x\e" ~~ /.* '\e'/, "a single-quoted '\\e' stays a backslash and an e";
ok "x\\e" ~~ /.* '\e'/, 'and matches those two characters';
