use Test;

plan 4;

# `.subst` publishes the substitution's captures to `$0`.. (not only `$/`).

"q" ~~ /(q)/;
my $x = "abc".subst(/(b)/, { $0 ~ $0 });
is $x, "abbc", 'closure replacement sees $0';
is ~$0, "b", '$0 after closure .subst is the substitution capture';

"q" ~~ /(q)/;
"abc".subst(/(b)/, 'X');
is ~$0, "b", '$0 after string-replacement .subst';

"q" ~~ /(q)/;
"abcb".subst(/(b)/, 'X', :g);
is ~$0, "b", '$0 after :g string-replacement .subst';
