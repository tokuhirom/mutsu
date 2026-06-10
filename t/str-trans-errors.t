use Test;

# .trans validates its arguments:
#  - a non-Pair positional argument throws X::Str::Trans::InvalidArg (got);
#  - a substitution key element that is not a Str/Regex throws
#    X::Str::Trans::IllegalKey (key).

plan 4;

throws-like '"a".trans(rx/a/)', X::Str::Trans::InvalidArg,
    'bare Regex argument', got => Regex;

throws-like '"a".trans([Any.new] => [Any.new])', X::Str::Trans::IllegalKey,
    'illegal substitution key', key => Any;

# Valid transliterations still work.
is "abc".trans("a" => "x"), "xbc", 'simple pair trans';
is "hello".trans(["el"] => ["ip"]), "hiplo", 'array-keyed (token) trans';
