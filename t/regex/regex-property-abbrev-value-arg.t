use Test;

# Issue #8486: rakudo does not accept every short Unicode property alias in a
# `<:prop(value)>` regex assertion, even when the long form works fine and
# the same abbreviation is accepted elsewhere (`.uniprop('Nv')`, say). `Nv`
# and `Nt` are the two mutsu got wrong -- it treated them as synonyms for
# `Numeric_Value`/`Numeric_Type`, but rakudo's own `<:Nv(1)>` and
# `<:Nt("Decimal")>` never match anything at all. Other abbreviated
# value-typed properties (sc, gc, bc, lb, ea, jt) already agree with rakudo
# and are not affected by this fix -- see the checks below.

plan 12;

# The abbreviations rakudo does NOT accept for a value-arg assertion: the
# whole thing silently fails to match rather than raising an error.
nok "ab1" ~~ / <:Nv(1)> /,            '<:Nv(1)> never matches (rakudo rejects the abbreviation)';
nok "1" ~~ / <:Nt("Decimal")> /,      '<:Nt("Decimal")> never matches, same gap';
nok "1" ~~ / <:Nt<Decimal>> /,        '...angle-bracket form too';

# The long forms still work correctly.
is ("ab1" ~~ / <:Numeric_Value(1)> /).Str, '1', '<:Numeric_Value(1)> matches the digit';
ok "1" ~~ / <:Numeric_Type("Decimal")> /, '<:Numeric_Type("Decimal")> matches a decimal digit';
ok "1" ~~ / <:Numeric_Type<Decimal>> /,   '...angle-bracket form too';

# A bare (no value) <:Nv> / <:Nt> already agreed with rakudo before this fix
# (both always False) and must keep doing so.
nok "1" ~~ / <:Nv> /,   'bare <:Nv> still never matches (unaffected, pre-existing agreement)';
nok "1" ~~ / <:Nt> /,   'bare <:Nt> still never matches (unaffected, pre-existing agreement)';

# The other abbreviated value-typed properties are unaffected by this fix.
ok "aA" ~~ / <:sc<Latin>> /,          'sc<Latin> still matches (unaffected)';
ok "A" ~~ / <:gc<Lu>> /,              'gc<Lu> still matches (unaffected)';
ok "a" ~~ / <:bc<L>> /,               'bc<L> still matches (unaffected)';
ok "a" ~~ / <:lb<AL>> /,              'lb<AL> still matches (unaffected)';
