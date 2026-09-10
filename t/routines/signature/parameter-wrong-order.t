use Test;
use MONKEY-SEE-NO-EVAL;

plan 9;

# X::Parameter::WrongOrder: a positional parameter may not follow an optional,
# named, or variadic parameter. `.misplaced` / `.after` describe the conflict.
throws-like 'sub f($a?, $b) { }', X::Parameter::WrongOrder,
    misplaced => 'required', after => 'optional',
    'required after optional positional';
throws-like 'sub f(*@a, $b) { }', X::Parameter::WrongOrder,
    misplaced => 'required', after => 'variadic',
    'required after variadic';
throws-like 'sub f(*@a, $b?) { }', X::Parameter::WrongOrder,
    misplaced => 'optional positional', after => 'variadic',
    'optional positional after variadic';
throws-like 'sub f(:$a, $b) { }', X::Parameter::WrongOrder,
    misplaced => 'required', after => 'named',
    'required after named';

# Legitimate signatures must NOT throw.
lives-ok { EVAL 'sub a($x, $y?) { }' },     'required then optional is fine';
lives-ok { EVAL 'sub b($x?, $y?) { }' },    'optional then optional is fine';
lives-ok { EVAL 'sub c($x, *@rest) { }' },  'positional then slurpy is fine';
lives-ok { EVAL 'sub d($x, :$named) { }' }, 'positional then named is fine';
lives-ok { EVAL 'sub e($x?, :$n) { }' },    'optional then named is fine';
