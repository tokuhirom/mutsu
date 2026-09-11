use Test;

plan 2;

is ("abcdn" ~~ /(. ** {2..3})+ n/).gist,
    "｢abcdn｣\n 0 => ｢ab｣\n 0 => ｢cd｣",
    'a backtracked capture keeps the block quantifier minimum';

is ("a,a" ~~ /^ (a **? {2} % ",") $/).gist,
    "｢a,a｣\n 0 => ｢a,a｣",
    'a frugal separated block quantifier keeps its exact count';
