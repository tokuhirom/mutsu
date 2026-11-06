use Test;
use lib 't/lib';
use LexMixinBase;

plan 3;

# A module's `my class` is registered under a mangled key. A mixin class made
# from it (`C.^mixin(R)`) must not make the bare name `LexBase` ambiguous: a
# `LexBase:D` parameter has to keep matching, and ranking narrower than `Array:D`.
my @a is LexRounded = <a b c>;
my $m := LexRounded.^mixin(role Extra { });
my $p = LexRounded[Str];
is kind(@a), 'LexBase', 'candidate on the lexical class wins once a mixin class of it exists';
ok @a ~~ LexRounded:D, 'the instance still matches the class constraint';
is kind(Array.new(1, 2)), 'Array', 'a plain Array still takes the Array candidate';
