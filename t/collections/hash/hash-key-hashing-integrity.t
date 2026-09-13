use Test;

# A Raku hash's key map is hashed with a randomly-seeded fast hasher rather than
# std's SipHash (ADR-0103 / #8333). Swapping a `BuildHasher` is invisible when it
# is right and silently wrong when the key type's `Hash`/`Eq`/`Borrow<str>` stop
# agreeing, so this pins the contract the swap has to keep: every key that went
# in comes back out, under exactly one entry, whatever its bytes.
#
# Raku specifies hash iteration ORDER as arbitrary, and mutsu's is genuinely
# nondeterministic per process, so nothing here asserts an order — it asserts the
# key SET, which is what the language actually promises.

plan 27;

# --- Keys that straddle every interesting boundary of the key representation ---
# 0 bytes, 1 byte, and either side of a 15-byte inline/heap split; ASCII,
# multi-byte UTF-8, digits, and keys sharing long common prefixes (the shape that
# separates a well-distributed hash from one that piles up in a single bucket).
my @keys =
    "",
    "a",
    "0",
    "aaaaaaaaaaaaaa",      # 14
    "aaaaaaaaaaaaaaa",     # 15
    "aaaaaaaaaaaaaaaa",    # 16
    "aaaaaaaaaaaaaaaaa",   # 17
    "key-with-a-quite-long-name-well-past-any-inline-capacity",
    "prefix-collide-0001",
    "prefix-collide-0002",
    "prefix-collide-0003",
    "\c[LATIN SMALL LETTER E WITH ACUTE]",
    "日本語のキー",
    "emoji-\c[SNOWMAN]-key",
    "space in key",
    "tab\tin\tkey",
    "newline\nin\nkey",
    "0 but true",
    "Inf",
    "NaN",
    ;

my %h;
for @keys.kv -> $i, $k {
    %h{$k} = $i;
}

is %h.elems, @keys.elems, "every distinct key got its own entry";

my $all-readable = True;
for @keys.kv -> $i, $k {
    $all-readable = False unless %h{$k} == $i;
}
ok $all-readable, "every key reads back the value it was stored with";

is %h.keys.sort.join("\0"), @keys.sort.join("\0"), ".keys is exactly the key set";
is %h.values.sort({ $^a <=> $^b }).join(","), (^@keys.elems).join(","),
   ".values is exactly the value set";
is %h.pairs.elems, @keys.elems, ".pairs yields one pair per entry";
is %h.pairs.map(*.key).sort.join("\0"), @keys.sort.join("\0"),
   ".pairs keys agree with .keys";

# A re-store must land on the SAME entry, not add a second one — this is where a
# `Hash`/`Eq` disagreement would show up as a duplicate key.
%h{"日本語のキー"} = 999;
is %h.elems, @keys.elems, "re-storing an existing key does not add an entry";
is %h{"日本語のキー"}, 999, "re-storing an existing key overwrites in place";

ok %h{""}:exists, ":exists finds the empty-string key";
ok %h{"aaaaaaaaaaaaaaa"}:exists, ":exists finds a 15-byte key";
ok %h{"aaaaaaaaaaaaaaaa"}:exists, ":exists finds a 16-byte key";
nok %h{"aaaaaaaaaaaaaaaaaa"}:exists, ":exists rejects a key that was never stored";
nok %h{"prefix-collide-0004"}:exists, ":exists rejects an unstored long-prefix sibling";

%h{"prefix-collide-0002"}:delete;
is %h.elems, @keys.elems - 1, "delete removes exactly one entry";
nok %h{"prefix-collide-0002"}:exists, "the deleted key is gone";
ok %h{"prefix-collide-0001"}:exists, "its prefix sibling survives the delete";
ok %h{"prefix-collide-0003"}:exists, "its other prefix sibling survives the delete";

# --- A copy must carry the whole key set across, hasher seed and all ---
my %copy = %h;
is %copy.elems, %h.elems, "a hash copy has the same number of entries";
is %copy.keys.sort.join("\0"), %h.keys.sort.join("\0"), "a hash copy has the same key set";
is %copy{"日本語のキー"}, 999, "a hash copy reads a multi-byte key correctly";

# --- Scale: enough keys to force several table growths/rehashes ---
my %big;
%big{"k$_"} = $_ * 2 for ^500;
is %big.elems, 500, "500 generated keys each got an entry";
my $big-ok = True;
for ^500 -> $i {
    $big-ok = False unless %big{"k$i"} == $i * 2;
}
ok $big-ok, "every one of 500 keys reads back correctly after table growth";
is %big.keys.elems, 500, ".keys over a grown table yields every key once";
is %big.keys.unique.elems, 500, ".keys over a grown table yields no duplicates";

# --- Object hashes keep their own side map of original keys ---
# `original_keys` is hashed the same way as the main map, so it needs the same
# guarantee: the WHICH string must find its way back to the real key object.
my %obj{Any};
%obj{1} = "int-one";
%obj{"1"} = "str-one";
%obj{1.0} = "rat-one";
is %obj{1}, "int-one", "an object hash distinguishes an Int key";
is %obj{"1"}, "str-one", "an object hash distinguishes a Str key with the same text";
ok %obj.keys.grep({ $_ ~~ Int }).elems >= 1, "an object hash keeps its Int key typed";
