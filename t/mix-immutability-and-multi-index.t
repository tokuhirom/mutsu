use Test;

plan 9;

my $m = mix <a foo a a a a b foo>;

is ~$m<a b>, "5 1", "Mix supports multi-key indexing";
is ~$m<a santa b easterbunny>, "5 0 1 0", "Mix multi-key indexing keeps missing keys as 0";

throws-like { $m<a> = 5 }, X::Assignment::RO, "Mix index assignment is read-only";
throws-like { $m<a>++ }, Exception, "Mix index increment is not allowed";
throws-like { $m<a>:delete }, X::Immutable, "Mix :delete is immutable";

throws-like { $m.keys = <c d> }, X::Assignment::RO, "Cannot assign to Mix.keys";
throws-like { $m.values = 3, 4 }, X::Assignment::RO, "Cannot assign to Mix.values";

is ~$m<a b>, "5 1", "Failed mutations do not change Mix contents";
isa-ok $m, Mix, "Value remains a Mix after failed mutations";
