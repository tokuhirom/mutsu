use Test;

plan 10;

# List.fmt formats each item with sprintf; a directive count that differs
# from the arguments each item supplies is an error (as in Rakudo).
throws-like { <a b>.fmt('%s%s') }, X::AdHoc,
    message => /'specify 2 arguments, but 1 argument was supplied'/, 'two directives, one item';
throws-like { (1, 2).fmt('x') }, X::AdHoc,
    message => /'specify 0 arguments, but 1 argument was supplied'/, 'no directive, one item';
throws-like { [1, 2].fmt('%d %d') }, X::AdHoc,
    message => /'specify 2 arguments'/, 'array item count mismatch';
throws-like { (a => 1).fmt('%s') }, X::Str::Sprintf::Directives::Count,
    args-used => 1, args-have => 2, 'Pair supplies key and value';
is (1, (a => 1)).fmt('%s'), "1 a\t1", 'a Pair item inside a list is one argument';

is ((a => 1), (b => 2)).fmt('%s=%s'), 'a=1 b=2', 'Pair items under a two-directive format are Pair.fmt';
throws-like { ((a => 1),).fmt('x') }, X::Str::Sprintf::Directives::Count,
    args-used => 0, args-have => 2, 'a Pair item under a mismatching format';

is <a b>.fmt('%s'), 'a b', 'matching count still works';
is (a => 1).fmt('%s=%s'), 'a=1', 'Pair with two directives';
is (1, 2).fmt('%03d', ','), '001,002', 'separator form still works';
