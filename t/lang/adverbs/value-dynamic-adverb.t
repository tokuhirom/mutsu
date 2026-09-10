use Test;

# A boolean-flag regex adverb (`:i`, `:s`, ...) is a compile-time switch and may
# not take a runtime/dynamic value. Passing a dynamic variable is X::Value::Dynamic.

plan 4;

throws-like 'm:i(@*ARGS[0])/foo/', X::Value::Dynamic;
throws-like 'm:s(@*ARGS[0])/foo/', X::Value::Dynamic;

# Plain boolean flags (no argument) and value adverbs still work.
ok (so 'FOO' ~~ m:i/foo/), ':i flag matches case-insensitively';
ok (so 'foofoofoo' ~~ m:x(3)/foo/), ':x(3) value adverb still works';
