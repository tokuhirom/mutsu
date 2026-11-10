use Test;

plan 8;

# `CORE::` is the setting's stash: its keys include the built-in routines as
# `&name`, so code that completes or inspects names (Jupyter::Kernel's
# autocompleter does `CORE::.keys.grep: { /^ '&'? "prin" / }`) finds them.

my @keys = CORE::.keys;

ok @keys.elems > 100, 'CORE:: lists many symbols';
ok '&say' ∈ @keys, '&say is listed';
ok '&print' ∈ @keys && '&printf' ∈ @keys, '&print and &printf are listed';
is-deeply @keys.grep({ /^ '&'? "prin" / }).sort.List, ('&print', '&printf'),
    'the completion query finds print and printf';
ok (CORE::<&say>:exists), 'an existing routine is found with :exists';
ok !(CORE::<&no-such-routine-here>:exists), 'an unknown routine is not';
ok CORE::<&say> ~~ Callable, 'the value is a callable';
is CORE::<&uc>.('ab'), 'AB', 'and it can be called';

done-testing;
