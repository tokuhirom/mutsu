use Test;

# The plain `.starts-with($needle)` / `.ends-with($needle)` form (a single
# positional argument on a Str receiver) is dispatched natively by the VM
# instead of falling back to the tree-walking interpreter. The case-/mark-
# insensitive named-arg forms keep falling through, so they must still work too.

plan 22;

# Basic prefix/suffix on a string variable and a literal.
my $s = "hello world";
ok $s.starts-with("hello"), 'variable receiver starts-with prefix';
ok $s.ends-with("world"),   'variable receiver ends-with suffix';
ok "hello".starts-with("he"), 'literal receiver starts-with prefix';
ok "hello".ends-with("lo"),   'literal receiver ends-with suffix';

nok $s.starts-with("world"), 'starts-with non-prefix is False';
nok $s.ends-with("hello"),   'ends-with non-suffix is False';

# Empty needle / empty string edge cases.
ok "abc".starts-with(""), 'starts-with empty needle is True';
ok "abc".ends-with(""),   'ends-with empty needle is True';
ok "".starts-with(""),    'empty string starts-with empty is True';
nok "abc".starts-with("abcd"), 'longer needle is False';

# Unicode.
ok "café".ends-with("fé"),   'ends-with multibyte suffix';
ok "café".starts-with("caf"), 'starts-with multibyte prefix';

# Non-string (Cool) receiver: coerced to Str (falls through to interpreter).
ok 42.starts-with("4"),  'Int receiver coerces and starts-with';
ok 42.ends-with("2"),    'Int receiver coerces and ends-with';

# Case-insensitive / mark-insensitive named-arg forms still work.
ok "Hello".starts-with("hello", :i), 'starts-with :i case-insensitive';
ok "WORLD".ends-with("rld", :i),     'ends-with :i case-insensitive';
ok "café".ends-with("cafe", :m),     'ends-with :m mark-insensitive';
nok "Hello".starts-with("hello"),    'starts-with without :i is case-sensitive';

# Needle coercion from a non-Str positional.
ok "42abc".starts-with(42), 'numeric needle coerces to Str';

# A user-defined .starts-with method is not shadowed by the native arm.
class D { method starts-with($x) { "user:$x" } }
is D.new.starts-with("z"), "user:z", 'user-defined starts-with takes precedence';

# Chained on the result of another method.
ok "  trim me  ".trim.starts-with("trim"), 'starts-with after .trim';
ok "ABCDEF".lc.ends-with("def"), 'ends-with after .lc';
