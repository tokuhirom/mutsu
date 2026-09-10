use Test;

plan 10;

# A literal parameter constrains the VALUE, not just the type the parser infers
# from it. Dispatch already honoured this; the reflected Signature did not, so
# a capture that only agreed on the type matched.

my $sig = :('greet', $name);

ok  (\('greet', 'world') ~~ $sig), "a capture with the literal matches";
nok (\('other', 'world') ~~ $sig), "a capture with a different Str does not";
nok (\('greet')          ~~ $sig), "too few positionals does not match";

# The same check through the explicit method form.
ok  $sig.ACCEPTS(\('greet', 'world')), "Signature.ACCEPTS agrees on a match";
nok $sig.ACCEPTS(\('other', 'world')), "Signature.ACCEPTS agrees on a mismatch";

# Numeric and boolean literals, not just strings.
my $nums = :(0, $rest);
ok  (\(0, 'x') ~~ $nums), "an Int literal parameter matches its value";
nok (\(1, 'x') ~~ $nums), "and rejects a different Int";

# A signature taken off a real routine keeps the literal too.
my &b = -> 'greet', $name { "hi $name" };
ok  &b.signature.ACCEPTS(\('greet', 'world')), "a block's signature keeps the literal";
nok &b.signature.ACCEPTS(\('bye', 'world')), "and rejects a non-matching capture";

# Dispatch itself is unchanged.
is &b('greet', 'world'), "hi world", "calling through the literal still works";
