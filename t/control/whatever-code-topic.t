use Test;

plan 14;

# A WhateverCode whose body mentions `$_` binds the element to its `*`
# placeholder, so `$_` keeps referring to the CALLER's topic. Only a bare block
# (`{ ... }`) topicalizes `$_` to the element.

my @a = <x y>;

{
    $_ = 'y';
    is @a.first(* eq $_), 'y', 'first: $_ in a WhateverCode is the outer topic';
    is @a.first(* eq $_, :k), 1, 'first :k: $_ in a WhateverCode is the outer topic';
    is-deeply @a.grep(* eq $_).List, ('y',), 'grep: $_ in a WhateverCode is the outer topic';
    is-deeply @a.map(* eq $_).List, (False, True), 'map: $_ in a WhateverCode is the outer topic';
}

{
    $_ = 'x';
    is @a.first(* eq $_), 'x', 'first: follows the outer topic when it changes';
    is-deeply @a.grep(* eq $_).List, ('x',), 'grep: follows the outer topic when it changes';
}

# A bare block still topicalizes to the element.
{
    $_ = 'y';
    is @a.first({ $_ eq 'x' }), 'x', 'first: a bare block topicalizes $_ to the element';
    is-deeply @a.grep({ $_ eq 'x' }).List, ('x',), 'grep: a bare block topicalizes $_ to the element';
}

# A WhateverCode with no `$_` in its body keeps `*` as the element.
is @a.first(*.chars == 1), 'x', 'first: a $_-less WhateverCode still receives the element';
is-deeply @a.grep(*.chars == 1).List, ('x', 'y'), 'grep: a $_-less WhateverCode still receives the element';

# The outer topic is the enclosing block's, not a leftover from a prior
# iteration of the loop that is doing the calling.
my @out = <p q>.map({ my @n = <p q>; @n.first(* eq $_) });
is-deeply @out.List, ('p', 'q'), 'nested: each iteration sees its own topic';

# No outer topic at all: `$_` is undefined inside the WhateverCode.
sub no-topic() { return @a.grep(* eq ($_ // 'none')).elems }
is no-topic(), 0, 'grep: no outer topic leaves $_ undefined';

# The zef shape this came from: a spec is kept unless some dist claims it.
class Dist {
    has $.name;
    method contains-spec($spec) { $spec eq $.name }
}
my @skip = Dist.new(name => 'Test::META');
my @specs = <Test META6 URI>;
is-deeply @specs.grep({ not @skip.first(*.contains-spec($_)) }).List,
    ('Test', 'META6', 'URI'),
    'nested first(*.method($_)) inside a grep block sees the grep topic';
my @skip2 = Dist.new(name => 'META6');
is-deeply @specs.grep({ not @skip2.first(*.contains-spec($_)) }).List,
    ('Test', 'URI'),
    'nested first(*.method($_)) filters the matching spec only';
