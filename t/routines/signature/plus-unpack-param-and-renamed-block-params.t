use Test;

plan 9;

# Test::Describe: `+[Int $first, *@rest]` is the single-argument-rule spelling of
# the `*[...]` slurpy unpack.
sub f(+[Int $first, *@rest]) { "$first|@rest[]" }
is f(1, 2, 3), '1|2 3', '+[..] unpacks a plain argument list';
is f((4, 5, 6)), '4|5 6', '+[..] unpacks a single list argument';
is f([7, 8]), '7|8', '+[..] unpacks a single array argument';

class M { multi method CALL-ME(+[Int $n, *@r], *%p) { "$n:@r[]" } }
is M.new()(9, 8), '9:8', '+[..] in a multi method signature';

# A pointy block may rename a named parameter, `-> :name(&alias)`.
my &cb = -> :counter(&c) { c() + 1 };
is cb(:counter(-> { 1 })), 2, 'pointy block :name(&alias)';
my &sc = -> :key($k) { $k * 2 };
is sc(:key(4)), 8, 'pointy block :name($alias)';
sub it($name, &b) { b(:counter(-> { 42 })) }
is (it "x", -> :counter(&c) { c() }), 42, 'renamed pointy block as a listop argument';

# The `&:name` placeholder is the callable counterpart of `$:name`.
my $d = { &:m() ~ "!" };
is $d(:m(-> { "hi" })), 'hi!', '&:name() placeholder call';
my $e = { &:m };
is $e(:m(-> { "yo" })).(), 'yo', '&:name placeholder value';
