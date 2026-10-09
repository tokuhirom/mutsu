use Test;
use lib 't/lib';
use AddMethodLexicalType;

# Code handed to ^add_method by a module keeps the module's `my class` / `my role`
# in scope when it later runs as a method of an unrelated class
# (found via the hide-methods distribution).
plan 2;

class Target { }
install-on(Target);

is Target.make-hidden, "hi", "a module-lexical class is visible in an added method";
nok Target.is-marked(42), "a module-lexical role is visible in an added method";
