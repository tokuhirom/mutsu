use Test;

plan 2;

sub id($v) { $v }

my $from_corner = id ｢hello world｣;
is $from_corner, 'hello world', 'corner-bracket string works in bareword call args';

my @target = $*DISTRO.is-win ?? «/c ""» !! '/dev/null';
is @target[0], $*DISTRO.is-win ?? '/c' !! '/dev/null', 'french quote-word list parses in expression';
