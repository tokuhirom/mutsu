use Test;

# The `.substr-eq($needle, $pos)` form with a plain non-negative Int position on
# a Str receiver is dispatched natively by the VM. Whatever / out-of-range /
# negative positions and the case-/mark-insensitive named-arg forms keep falling
# through to the interpreter.

plan 16;

ok  "hello".substr-eq("ell", 1),   'matching substring at position';
nok "hello".substr-eq("ell", 2),   'non-matching substring at position';
ok  "hello".substr-eq("hello", 0), 'full match at position 0';
ok  "hello".substr-eq("lo", 3),    'match at tail position';
nok "hello".substr-eq("xx", 1),    'no match';
ok  "hello".substr-eq("", 2),      'empty needle matches anywhere in range';
ok  "hello".substr-eq("o", 4),     'single char at last position';

# Position equal to length with empty needle is in range.
ok  "abc".substr-eq("", 3),        'empty needle at position == length';

# Unicode.
ok  "café au lait".substr-eq("au", 5), 'multibyte substring match';
ok  "café".substr-eq("fé", 2),         'multibyte tail match';

# Numeric needle coerces to Str.
ok  "a42b".substr-eq(42, 1),       'numeric needle coerces';

# Case-insensitive named-arg form (falls through to the interpreter).
ok  "hello".substr-eq("ELL", 1, :i), 'substr-eq :i case-insensitive';
nok "hello".substr-eq("ELL", 1),     'substr-eq without :i is case-sensitive';

# Position from a Whatever (falls through; interpreter resolves it).
ok  "hello".substr-eq("lo", *-2),  'substr-eq with Whatever position';

# A user-defined .substr-eq method is not shadowed by the native arm.
class S { method substr-eq($a, $b) { "user:$a:$b" } }
is S.new.substr-eq("x", 1), "user:x:1", 'user-defined substr-eq takes precedence';

# Chained on a method result.
ok "  hello  ".trim.substr-eq("ell", 1), 'substr-eq after .trim';
