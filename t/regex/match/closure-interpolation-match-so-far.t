use Test;

plan 3;

my @log;
"abc" ~~ / a <{ @log.push(~$/); 'b' }> c /;
is @log.join(","), "a", '$/ inside <{ }> is the match so far';

my @log2;
"xabc" ~~ / a <{ @log2.push(~$/); 'b' }> c /;
is @log2.join(","), "a", 'match so far starts at the match start, not the subject start';

ok "abc" ~~ / (a) <{ $0 eq 'a' ?? 'b' !! 'zzz' }> c /, 'positional capture visible to <{ }>';
