use Test;

plan 8;

'abcb'.subst(/(b)/, 'X');
is $/[0].from, 1, 'a string replacement keeps the first capture start';
is $/[0].to, 2, 'a string replacement keeps the first capture end';

'abcb'.subst(/(b)/, 'X', :g);
is $/[1][0].from, 3, 'a global replacement keeps the later capture start';
is $/[1][0].to, 4, 'a global replacement keeps the later capture end';
ok $/.raku.starts-with('$('), 'the global match list is itemized';

'abcb'.subst(/$<letter>=(b)/, 'X', :g);
is $/[1]<letter>.from, 3, 'a named capture retains its subject offset';

my $topic = 'abcb';
$topic ~~ s:g/(b)/X/;
is $/[1][0].from, 3, 'the substitution operator shares the span-bearing match';

'abcd'.subst(/(z)/, 'X', :g);
is $/.raku, '$( )', 'an unmatched global substitution leaves an itemized empty list';
