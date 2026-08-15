use v6;
use Test;

# An object-hash (`%h{Str:D}`) declared with an inline initializer keeps its
# `is default(...)` metadata when later bound through a `for`-loop's
# positional sub-signature destructuring (`-> ($name, %b is raw) { }`).
# Regression: `coerce_hash_var_value` rebuilt a fresh `HashData` for the
# `.WHICH`-keyed re-key step without carrying over `value_type`/`key_type`/
# `declared_type`/`default` from the source hash, so `%b.VAR.default`
# silently became `(Any)` (roast/S02-names/is_default.t test 113).

plan 2;

my %h{Str:D} is default(42) = o => 768;
is %h.VAR.default, 42, 'object-hash keeps its own default';

my @list = (("t", %h),);
for @list -> ($name, %b is raw) {
    is %b.VAR.default, 42, 'destructured %b keeps the source default';
}
