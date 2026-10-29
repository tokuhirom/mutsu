use Test;

# A `.name(` inside a double-quoted regex literal is plain text unless an
# interpolated variable precedes it (`"$o.uc()"`). `"d.add('number', '" $n "')"`
# (JavaScript::Google::Charts t/scatter-column-names) used to read the `(` as a
# method-call argument list, so the literal's closing quote was lost.

plan 9;

my $n = 'x';
ok "d.add('number', 'x')" ~~ /"d.add('number', '" $n "')"/, 'plain .add( text then an interpolated variable';
ok "d.add(x" ~~ /"d.add(" $n/, 'a literal ending in .add( before a variable';
ok "a.b(x)" ~~ /"a.b(" $n ")"/, 'a.b( then variable then )';
ok "a.b(" ~~ /"a.b("/, 'a literal ending in an unmatched paren';
ok "x\nd.add('number', 'x');\ny" ~~ /"d.add('number', '" $n "')" /, 'in a multi-line subject';

my @a = <x y>;
my $o = 'ab';
ok "x,y" ~~ /"@a.join(",")"/, 'an interpolated array method call still works';
ok "AB" ~~ /"$o.uc()"/, 'an interpolated scalar method call still works';
ok "ab" ~~ /"$o.substr(0,1)b"/, 'a method call with arguments still works';
nok "zzz" ~~ /"d.add(" $n/, 'and a non-match is still a non-match';
