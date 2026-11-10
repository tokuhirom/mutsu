use Test;

plan 6;

# CORE:: lists the core types and routines (`&print`, `&say`, ...).
my @keys = CORE::.keys;
ok @keys > 100, "CORE::.keys is populated";
ok @keys.grep('&print'), "&print is a CORE key";
ok @keys.grep('&say'), "&say is a CORE key";
ok @keys.grep('Int'), "Int is a CORE key";
is CORE::.keys.grep(/^'&prin'/).sort.head(2).List, ('&print', '&printf'), "grep by prefix";
ok CORE::<&say> === &CORE::say, "the listed routine is the CORE one";
