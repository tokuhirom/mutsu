use Test;

# §2 builtin-MRO all-candidates dispatch: `.+`/`.*` on a built-in type must yield
# one result per MRO level that defines the method. `List.elems` and `Any.elems`
# both define elems, so `<a b>.+elems` is (2, 2). Data-driven from the per-type
# method tables (builtin_type_method_names) intersected with the type's MRO.
# Mirrors roast/S03-metaops/hyper.t #407.

plan 8;

# Bare `.+`/`.*` on a List: List.elems + Any.elems = 2 candidates, both 2.
is-deeply <a b>.+elems, (2, 2), '.+elems on a List yields both MRO candidates';
is-deeply <a b>.*elems, (2, 2), '.*elems on a List yields both MRO candidates';

# The result is a List (parens), not an Array.
ok <a b>.+elems ~~ List, '.+ returns a List';

# Hyper `».+`/`».*` applies per element.
my @a := <a b>, <c d e>;
is-deeply @a».+elems, ((2, 2), (3, 3)), '».+elems (hyper, all candidates)';
is-deeply @a».*elems, ((2, 2), (3, 3)), '».*elems (hyper, all candidates)';

# Coderef / qualified forms are single-candidate.
is-deeply @a».+&elems, ((2,), (3,)), '».+& is single-candidate';
is-deeply @a».+Any::elems, ((2,), (3,)), '».+:: (qualified) is single-candidate';

# `.*` on a method the type does not have yields the empty list.
is-deeply 42.*no-such-method-xyz, (), '.* on a missing method is ()';
