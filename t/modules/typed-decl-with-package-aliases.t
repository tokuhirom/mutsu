use v6;
use lib 't/lib-typed-decl-alias-gate';
use Test;

# Once a loaded module has given the program a package alias table, a typed
# declaration of a builtin name nothing aliases (`my int $i`, `my Int $n`)
# still answers from its per-site memo, and a module's own nested class that
# shares a builtin's short name (`Str` inside the module) still names that class.

use TdAlias::Shapes;

plan 6;

is Point.new.x, 1, 'the exported class';
is TdAlias::Shapes::local-str(), 'nested Str', "the module's nested Str inside its routine";
is TdAlias::Shapes::plain-int(5), 10, 'a typed Int accumulator in the module';

my int $acc = 0;
loop (my int $i = 0; $i < 1000; $i++) {
    my int $even = $i %% 2;
    $acc += $even;
}
is $acc, 500, 'a native int declared inside a loop';
my Str $s = 'core';
is $s.^name, 'Str', 'outside the module Str is the core type';
throws-like { my int $bad = 'x' }, Exception, 'the native constraint is enforced';
