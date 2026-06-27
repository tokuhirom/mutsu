use Test;
use MONKEY-SEE-NO-EVAL;

plan 6;

# The X::Adverb raised by a zen slice (`@a[]`) carrying a conflicting or unknown
# adverb reports `.what` = "zen slice", distinct from a whatever slice (`@a[*]`,
# "whatever slice"). Both yield all elements for the value adverbs.

my @a = <a b c d>;

throws-like '@a[]:k:v',  X::Adverb, what => 'zen slice',      'zen slice conflict → zen slice';
throws-like '@a[]:foo',  X::Adverb, what => 'zen slice',      'zen slice unknown adverb → zen slice';
throws-like '@a[*]:k:v', X::Adverb, what => 'whatever slice', 'whatever slice conflict → whatever slice';

# Value adverbs on a zen slice still yield all keys/values.
is-deeply (@a[]:k), (0, 1, 2, 3),                 'zen slice :k yields all keys';
is-deeply (@a[]:v), ("a", "b", "c", "d"),         'zen slice :v yields all values';
is-deeply (@a[]:kv), (0,"a",1,"b",2,"c",3,"d"),   'zen slice :kv';
