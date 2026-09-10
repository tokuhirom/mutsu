use v6;
use Test;

# Regression: a `my sub` declared inside a non-unit `package`/`module` block is
# lexically scoped and compile-time-visible throughout that block, so an earlier
# statement may forward-reference it. mutsu compiled the package body without
# hoisting sub declarations, so `package P { f(); my sub f {…} }` failed with
# "Unknown function". Surfaced loading Zef::CLI, whose `package Zef::CLI { … }`
# calls `preprocess-args-verbosity-mutate(@*ARGS)` above its `my sub` definition.

plan 4;

package P {
    our $result = greet('world');
    my sub greet($who) { "hello $who" }
}
is $P::result, 'hello world', 'forward reference to my sub inside a package block';

module M {
    our $r = twice(21);
    my sub twice($n) { $n * 2 }
}
is $M::r, 42, 'forward reference to my sub inside a module block';

# Mutual / later use of an already-defined sub still works (no regression).
package Q {
    my sub a() { b() + 1 }
    my sub b() { 10 }
    our $v = a();
}
is $Q::v, 11, 'sub calling a later-defined sibling sub in a package block';

# A nested package block hoists its own subs independently.
package Outer {
    our $top = inner-helper();
    my sub inner-helper() { 'outer-helper' }
}
is $Outer::top, 'outer-helper', 'nested package hoisting is self-contained';
