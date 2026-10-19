use Test;

plan 9;

my $c = $*SPEC.curupdir;
is $c.^name, 'Block', 'curupdir is a Block';
is $c.arity, 1, 'arity 1';
is ('a' ~~ $c), True, 'smartmatch accepts a name';
is ('.' ~~ $c), False, 'smartmatch rejects .';
is ('..' ~~ $c), False, 'smartmatch rejects ..';
is-deeply <. .. a b>.grep($c).List, <a b>.List, 'grep with curupdir';
is $c('x'), True, 'callable';
is $*SPEC.curupdir.ACCEPTS('.git'), True, 'ACCEPTS .git';
is IO::Spec::Unix.curupdir.ACCEPTS('..'), False, 'class-level call';
