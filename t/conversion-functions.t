use Test;
plan 12;

is chr(65), "A", "chr returns the codepoint's character";
is ord("Z"), 90, "ord returns the first codepoint";
is chrs(72, 105, 33), "Hi!", "chrs puts together codepoints";
is ords("Hi!")[0], 72, "ords returns the first codepoint";
is ords("Hi!")[2], 33, "ords returns the third codepoint";
is item(123), 123, "item returns its argument";
is elems(list(1, [2, 3])), 3, "list flattens arguments";
is lol([1, 2], 3)[0][1], 2, "lol keeps nested lists";
is hash(pair("a", 1), pair("b", 2))["a"], 1, "hash stores pairs";
is flat("a".."c" Z 1..3).hash["a"], 1, ".hash converts pair-like sequences";
dies-ok { hash(<1 2 3>) }, "hash dies on odd number of elements";
is gist(1.5), "1.5", "gist stringifies the value";
