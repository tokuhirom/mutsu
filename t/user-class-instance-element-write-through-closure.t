BEGIN %*ENV<MUTSU_REAL_TEST> = '1';
use Test;

plan 3;

class Map is Hash { }

my $map = Map.new;
lives-ok { $map<a> = 1 }, 'element assignment through real Test callback lives';
is $map<a>, 1, 'element assignment through callback reaches the caller scalar';

my $direct = Map.new;
$direct<a> = 2;
is $direct<a>, 2, 'direct user-class element assignment remains writable';
