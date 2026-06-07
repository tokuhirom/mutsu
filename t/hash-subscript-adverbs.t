use Test;

plan 14;

my %h = a => 1, b => 2, c => 3, d => 4;

# --- Zen-slice adverbs (`%h{}:adverb`) ---
is (%h{}:k).sort,  <a b c d>,          'zen slice :k';
is (%h{}:v).sort,  (1, 2, 3, 4),       'zen slice :v';
is (%h{}:kv).sort, (1, 2, 3, 4, "a", "b", "c", "d"), 'zen slice :kv';

# --- Range key as a multi-key hash slice ---
is %h{"b".."c"},        (2, 3),               'range key value slice';
is (%h{"b".."c"}:k),    <b c>,                'range key :k';
is (%h{"b".."c"}:kv),   ("b", 2, "c", 3),     'range key :kv';

# --- `.name` on array/hash variables returns the sigil'd name ---
is %h.name, '%h', 'hash .name returns sigil name';
my @arr = 1, 2;
is @arr.name, '@arr', 'array .name returns sigil name';

# --- X::Adverb on invalid / conflicting subscript adverbs ---
throws-like '%h{}:foo', X::Adverb,
    :what('{} slice'), :unexpected<foo>, 'unknown zen-slice adverb';
throws-like '%h{}:k:v', X::Adverb,
    :what<slice>, :nogo(<k v>), 'conflicting zen-slice adverbs';
throws-like '%h<a>:foo', X::Adverb,
    :unexpected<foo>, 'unknown adverb on a single-key subscript';
throws-like '%h{}:kv:p:zip:zop', X::Adverb,
    :nogo(<kv p>), :unexpected({ m/zip/ && m/zop/ }),
    'multiple nogo + unexpected adverbs';

# .source carries the variable name
throws-like '%h{}:foo', X::Adverb, :source('%h'), 'X::Adverb .source is the var name';

# A valid combination does not throw
lives-ok { %h{}:kv }, 'valid zen-slice adverb does not throw';
