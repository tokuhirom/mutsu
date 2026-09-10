use v6.c;
use Test;

plan 12;

# CORE:: pseudo-package exposes the core symbol table lazily. The `&CORE::name`
# symbolic form already worked; these pin the stash forms that resolve missing
# keys against the core resolver (roast S02-names pseudo-6c tests 60/61/63/66).
my $real = &not;

ok &CORE::not === $real,           '&CORE::not works';
ok CORE::.<&not> === $real,        'CORE::.<&not> stash form works';
my $core = "CORE";
ok ::($core)::('&not') === $real,  '::("CORE") indirect stash form works';
ok CORE::<&not> === $real,         'CORE::<&not> angle form works';

{
    sub not($x) { $x } #OK
    # CORE:: must bypass a same-named user shadow.
    ok CORE::.<&not> === $real,     'CORE::.<&not> ignores a user shadow';
    ok ::($core)::('&not') === $real, '::("CORE") ignores a user shadow';
}

# GLOBAL:: / OUR:: are package namespaces and do NOT contain CORE symbols
# (roast pseudo-6c test 81). A builtin qualified through them is undefined.
ok !defined(&GLOBAL::say),  'GLOBAL:: does not find CORE symbols';
ok !defined(&GLOBAL::not),  'GLOBAL:: does not find CORE &not';
ok !defined(&OUR::say),     'OUR:: does not find CORE symbols';

# DYNAMIC:: with an unfound name yields an undefined value, never an error, even
# through a deep repeated chain (roast pseudo-6c "no guts spillage").
ok !defined($DYNAMIC::nonexistent), 'unfound $DYNAMIC:: is undefined';
lives-ok { EVAL('$' ~ "DYNAMIC::" x 50 ~ 'True') },
    'deep $DYNAMIC:: chain lives (no guts spillage)';
is (EVAL('$' ~ "DYNAMIC::" x 50 ~ 'Nonexistent')).defined, False,
    'deep $DYNAMIC:: chain to unfound name is undefined';
