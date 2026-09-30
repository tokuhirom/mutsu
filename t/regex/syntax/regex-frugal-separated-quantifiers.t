use Test;

plan 11;

is ("a,a,a" ~~ / a+? % "," /).gist, '｢a｣', 'frugal one-or-more stops after the first atom';
is ("a,a,a" ~~ / a*? % "," /).gist, '｢｣', 'frugal zero-or-more tries zero first';
is ("a,a,a" ~~ / a**?2..3 % "," /).gist, '｢a,a｣', 'frugal counted range stops at its minimum';
is ("a,a" ~~ / a**?2..3 % "," /).gist, '｢a,a｣', 'frugal counted range accepts exactly its minimum';
is ("a" ~~ / a**?2..3 % "," /).gist, 'Nil', 'frugal counted range rejects too few atoms';
is ("a,a,a" ~~ / ^ a+? % "," ",a" /).gist, '｢a,a｣', 'following token can grow a frugal chain';
is ("a,a,a" ~~ / ^ a**?2..3 % "," $ /).gist, '｢a,a,a｣', 'end anchor can grow a frugal counted chain';
is ("a,a,a" ~~ / :r a+? % "," /).gist, '｢a｣', 'ratchet keeps the minimum frugal chain';
is ("a,a,a" ~~ / :r a*? % "," /).gist, '｢｣', 'ratchet keeps the zero-iteration frugal chain';

my $captured = "a,a,a" ~~ / (a)+? % "," /;
is $captured.gist, '｢a｣' ~ "\n 0 => ｢a｣", 'capturing separated quantifier stops at the first atom';
is ("a,a,a" ~~ / ^ (a)+? % "," ",a" /).gist, '｢a,a｣' ~ "\n 0 => ｢a｣",
    'capturing separated quantifier grows for following tokens';
