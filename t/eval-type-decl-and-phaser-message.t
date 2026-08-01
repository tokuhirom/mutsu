use Test;

# Two exception shapes rakudo's real `Test.rakumod` checks and mutsu got wrong.

plan 9;

use MONKEY-SEE-NO-EVAL;

# 1. A type name is legitimate in call position -- `99 but R("x")` initializes a
#    role's single public attribute -- so a class/role/subset declared in the
#    same EVAL'd unit counts as declared. It used to be reported as an
#    undeclared routine before the unit ran.
lives-ok { EVAL 'my role R { has $.x }; 99 but R("ok")' },
    'a role declared in the EVAL\'d unit may be called with an init value';

is EVAL('my role R { has $.x }; (99 but R("ok")).x'), 'ok',
    'and the init value reaches the attribute';

throws-like 'my role R { }; 99 but R("wrong")', X::Role::Initialization,
    'a role with no public attribute still rejects an init value';

throws-like 'my role R { has $.x; has $.y }; 99 but R("wrong")',
    X::Role::Initialization,
    'and so does one with two';

lives-ok { EVAL 'my subset Small of Int where * < 10; my Small $n = 3' },
    'a subset declared in the EVAL\'d unit is not an undeclared routine';

# The check still fires for a genuinely undeclared routine.
throws-like 'no_such_routine_at_all()', X::Undeclared::Symbols,
    'an undeclared routine in EVAL\'d code is still reported';

# 2. X::Phaser::PrePost carries the message raku builds from the phaser and its
#    condition, not an empty string.
throws-like 'my sub a { PRE 0 }; a()', X::Phaser::PrePost,
    message => /:s Precondition .0. failed/,
    'a failing PRE reports its precondition';

throws-like 'my sub a { POST 0 }; a()', X::Phaser::PrePost,
    message => /:s Postcondition .0. failed/,
    'a failing POST reports its postcondition';

throws-like 'my sub a { PRE 0 }; a()', X::Phaser::PrePost,
    phaser => 'PRE', condition => /0/,
    'and still carries .phaser and .condition';
