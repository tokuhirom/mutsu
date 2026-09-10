use Test;

plan 13;

# A bare Whatever passed as a *call argument* is not a curry point; invoking the
# `+` op routine with it numifies the Whatever, which dies (it does not make a
# closure). `*(42)` likewise invokes a bare Whatever (no CALL-ME) and dies.
dies-ok { &infix:<+>(*, 42) }, '&infix:<+>(*, 42) dies (not a closure)';
dies-ok { &infix:<R+>(*, 42) }, '&infix:<R+>(*, 42) dies (not a closure)';
dies-ok { *(42) }, '*(42) invokes a bare Whatever and dies';

# WhateverCode parameters are `is raw`: a mutating placeholder writes back to the
# caller's container.
{
    my $*f = 1;
    my $*g = 2;
    my sub f ($i) { $i($*f) }
    my sub g ($i) { $i($*g) }
    my sub fg ($i) { $i($*f, $*g) }
    lives-ok { f(*++); g(*--); fg(*++ + *--) }, 'rw WhateverCode calls live';
    is "$*f $*g", '3 0', 'WhateverCode parameters are rw';
}

# `*.foo` / read-only currying still curries and stays a Code.
isa-ok (* + 1), Code, '* + 1 is a WhateverCode';
is (* + 1)(41), 42, '* + 1 invokes correctly';

# `*++` / `++*` curry into a Code.
isa-ok (*++), Code, '*++ is some kind of code';
isa-ok (++*), Code, '++* is some kind of code';

# A WhateverCode stored in a `deepmap` leaf mutates through the leaf container.
{
    my @a = (10, 20);
    my @r = @a.deepmap(*--);
    is @r[0], 10, 'deepmap(*--) returns the old value';
    is @a[0], 9, 'deepmap(*--) mutates the source leaf';
}

# A pointy block can take a `+@` single-argument-rule slurpy parameter.
{
    my $b = -> +@foo { @foo.elems };
    is $b(1, 2, 3), 3, 'pointy block with +@ slurpy parameter';
}

# A WhateverCode that interpolates an outer lexical only inside a stored regex
# still captures that lexical (closure free-variable analysis scans regex
# literals).
{
    my @rx = <foo bar>.map(-> $r { * ~~ /<$r>/ });
    is @rx[0]('foo').so, True, 'regex `<$r>` in WhateverCode captures outer param';
}
