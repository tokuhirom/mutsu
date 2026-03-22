use Test;

plan 5;

# TODO: Mix and MixHash share the same Value::Mix variant, so we cannot
# enforce immutability on Mix without also blocking MixHash mutations.
# When a separate MixHash variant is added, restore immutability tests.

my $m = mix <a foo a a a a b foo>;

is ~$m<a b>, "5 1", "Mix supports multi-key indexing";
is ~$m<a santa b easterbunny>, "5 0 1 0", "Mix multi-key indexing keeps missing keys as 0";

throws-like { $m<a>:delete }, X::Immutable, "Mix :delete is immutable";

throws-like { $m.keys = <c d> }, X::Assignment::RO, "Cannot assign to Mix.keys";
throws-like { $m.values = 3, 4 }, X::Assignment::RO, "Cannot assign to Mix.values";
