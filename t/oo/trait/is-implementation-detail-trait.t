use Test;

# `is implementation-detail` on a `sub` (#9818): `Code.is-implementation-detail`
# reads the trait back. Ground truth against `raku`:
#
#   sub P is implementation-detail {}; say &P.is-implementation-detail; # True
#   say &say.is-implementation-detail;                                 # False
#
# Before this fix the trait was silently dropped by the parser (excluded from
# both its own flag and `custom_traits`), and `.is-implementation-detail`
# itself was not a recognised method at all -- it fell into the ADR-0070
# callable-compose fallback for a `Sub` target (`&<composed-method:...>`) and
# raised "No such method" outright for a `Routine`-shaped builtin like `&say`.

plan 3;

sub P is implementation-detail { }
ok &P.is-implementation-detail,
        'a sub declared `is implementation-detail` answers True';

sub Q { }
nok &Q.is-implementation-detail,
        'an ordinary sub answers False';

nok &say.is-implementation-detail,
        'a builtin with no registered FunctionDef answers False, not an error';

done-testing;
