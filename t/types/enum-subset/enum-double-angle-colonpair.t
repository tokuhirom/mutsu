use Test;

# A `«...»` / `<<...>>` quote-word enum body may contain colonpair variants
# with a PARENTHESIZED value (`:one(1)`). Previously the value was parsed with
# the full expression parser on the raw remainder, which swallowed the closing
# `»`/`>>` delimiter as a hyper operator (`«:one(1)»` mis-parsed as `(1)».(…)`),
# so the enum declaration failed entirely. Now only the balanced-paren content
# is parsed.

plan 9;

enum A «:one(1) two three four»;
is-deeply A.enums, Map.new((one => 1, two => 2, three => 3, four => 4)),
    'colonpair(paren) value then auto-incrementing bare words';
is two.value, 2, 'bare word after a valued colonpair auto-increments';

enum B «:one(1)»;
is-deeply B.enums, Map.new((:one(1))), 'single parenthesized colonpair';

enum C «two :one(1)»;
is-deeply C.enums, Map.new((two => 0, one => 1)), 'bare word before a valued colonpair';

enum D «:a(5) b :c(9)»;
is-deeply D.enums, Map.new((a => 5, b => 6, c => 9)), 'mixed valued colonpairs and bare word';

# ASCII << >> form.
enum E <<:one(1) two three>>;
is-deeply E.enums, Map.new((one => 1, two => 2, three => 3)), 'ASCII << >> colonpair form';

# An expression value inside the parens.
enum F «:one(1+2) two»;
is-deeply F.enums, Map.new((one => 3, two => 4)), 'expression value in colonpair parens';

# The angle-bracket colonpair value (`:one<x>`) still works.
enum G «:one<x> two»;
is-deeply G.enums, Map.new((one => 'x', two => 'y')), 'angle-value colonpair still works';

# Plain word-quote enum (no colonpair) is unaffected.
enum H «a b c»;
is-deeply H.enums, Map.new((a => 0, b => 1, c => 2)), 'plain word-quote enum unaffected';

done-testing;
