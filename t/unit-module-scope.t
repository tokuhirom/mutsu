use lib $*PROGRAM.parent.add('lib');
use Test;
use UnitModuleScope;

plan 11;

# A `unit module`'s routines belong to that module's package. Only `is export`
# ones reach the consumer; `my`, `our` and plain `sub` all stay put.
is call-all(), 'plain(x) our(x) my(x)',
    'a module routine reaches its siblings by their bare names';

for <plain-helper our-helper my-helper> -> $name {
    nok (try ::("&$name")).defined,
        "non-exported `$name` does not leak into the consumer's scope";
}

# The `our` variable is still reachable by its package-qualified name.
is $UnitModuleScope::pkg-var, 42, 'an `our` variable keeps its package name';

# `once` in a module sub fires once, not once per dispatch path. The first call
# resolves and OTF-compiles; later calls hit the name-keyed body cache, and both
# must run under the module's package so they agree on the `once` site key.
is bump-once(), 1, 'once in a module sub fires on the first call';
is bump-once(), 1, 'once does not re-fire on the second call';
is bump-once(), 1, 'once does not re-fire on the third call';

# An exported `proto` is visible to the consumer both as a callable and by name.
ok (try &pick).defined, 'an exported proto is visible as `&name`';
is pick(3), 'int(3)', 'the exported proto dispatches to its Int candidate';
is pick('a'), 'str(a)', 'the exported proto dispatches to its Str candidate';
