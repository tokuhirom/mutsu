use Test;

# A hyper method call was invisible to the Whatever-currying machinery, so
# `*.comb».uc` evaluated `Whatever.comb».uc` eagerly and produced a List instead
# of a WhateverCode. `Digest::RIPEMD` builds its round tables with
# `given *.comb».parse-base(16)`, which died with "No such method 'CALL-ME' for
# invocant of type 'List'".

plan 10;

isa-ok (*.comb».uc), Callable, 'a hyper method call curries into a WhateverCode';
is (*.comb».uc).WHAT.gist, '(WhateverCode)', '... reported as WhateverCode';

{
    my $g = *.comb».uc;
    is $g("abc").join(','), 'A,B,C', 'the curried closure applies the hyper call';
}

{
    my $h = *.comb>>.parse-base(16);
    is $h("1A").join(','), '1,10', 'the ASCII spelling curries too';
}

is (*.comb».uc)("xy").join(','), 'X,Y', 'the closure is callable straight away';

is <ab cd>.map(*.comb».uc).map(*.join('')).join(','), 'AB,CD',
    'it composes as a map block';

is (*.comb».parse-base(16))("0123ABCD").join(','), '0,1,2,3,10,11,12,13',
    'the shape Digest::RIPEMD uses';

is (do given *.comb».parse-base(16) { .("12AB") }).join(','), '1,2,10,11',
    '... reached through `given` and `.()`';

# Chained past the hyper call, and with an argument.
is (*.comb».uc.join('-'))("abc"), 'A-B-C', 'a plain method call after the hyper call';
is (*.words».chars)("aa bbb").join(','), '2,3', 'a hyper call with no arguments';
