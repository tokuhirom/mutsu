use lib 't/lib';
use Test;
use MultiWhereFileConstant;

plan 7;

class Composed does WRole { }

is WCls.new.mm(1, :style<ascii>), 'mm ascii', 'class multi method candidate reads the module constant';
is Composed.new.rmm(1, :style<ascii>), 'rmm ascii',
    'role multi method composed in the importing unit reads the module constant';
is Composed.new.rmm(1), 'rmm double', 'the default satisfies the constant too';
is s(1, :style<ascii>), 'sub ascii', 'single sub where reads the module constant';
is ms(1, :style<ascii>), 'msub ascii', 'exported multi sub candidate reads the module constant';
is ms(1), 'msub double', 'default value passes the multi sub where';
throws-like { ms(1, :style<nope>) }, X::Multi::NoMatch,
    'a value outside the constant is still rejected';
