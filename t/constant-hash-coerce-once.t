use Test;
use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test::Util;

# An `our`/implied-scope `constant %h = Obj` must coerce its value to a Map by
# calling `.Map` exactly once -- not twice (regression: the global store used to
# re-coerce the raw initializer after the local store had already coerced it,
# invoking the side-effecting `.Map` twice).

plan 3;

is_run 'class Foo { method Map { print "M"; Map.new: (:1a) } };'
     ~ 'constant %h = Foo.new;',
    { :out("M"), :err(''), :0status },
    'implied-scope constant calls .Map exactly once';

is_run 'class Foo { method Map { print "M"; Map.new: (:1a) } };'
     ~ 'our constant %h = Foo.new;',
    { :out("M"), :err(''), :0status },
    'our-scope constant calls .Map exactly once';

is_run 'class Foo { method Map { print "M"; Map.new: (:1a) } };'
     ~ 'my constant %h = Foo.new;',
    { :out("M"), :err(''), :0status },
    'my-scope constant calls .Map exactly once';
