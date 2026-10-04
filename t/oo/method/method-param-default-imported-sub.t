use Test;

# A method parameter default that calls a sub imported by the method's own
# file (`use Helper;` at file scope) must resolve in the method's lexical
# scope, not the caller's. mutsu died "Unknown function: default-label" because
# the default was evaluated before the method's routine frame was pushed.
# Found via ORM::ActiveRecord (`:$name = default-connection()` in DB.current).

plan 4;

use lib 't/lib';
use MethodParamDefaultUser;

is MethodParamDefaultUser.pick, 'primary', 'default calling an imported sub (type object)';
is MethodParamDefaultUser.new.pick, 'primary', 'default calling an imported sub (instance)';
is MethodParamDefaultUser.pick(:name<x>), 'x', 'a supplied named arg still wins';
is MethodParamDefaultUser.pick-pos(1), '1:primary', 'with a positional and a return type';
