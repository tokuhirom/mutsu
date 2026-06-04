use Test;

# A hyper infix operator invoked as a named function -- `infix:<»+«>(a, b)`,
# `&infix:<»+«>(a, b)`, or `&[»+«](a, b)` -- must return the same value as the
# operator form `a »+« b`, including the List container kind when the operands
# are Lists (not real Arrays). Previously the function form returned an Array,
# so `is-deeply` against a List literal failed. Mirrors the "can autogen"
# subtests in roast/S03-metaops/hyper.t.

plan 14;

# operator form is the reference
is-deeply ((1, 2, 3) »+« (4, 5, 6)), (5, 7, 9), 'operator form returns a List';

# named function form
is-deeply infix:<»+«>((1, 2, 3), (4, 5, 6)), (5, 7, 9), 'infix:<»+«> as function';
is-deeply infix:<»+»>((1, 2, 3), 1),         (2, 3, 4), 'infix:<»+»> right-dwim scalar';
is-deeply infix:<«+«>(1, (4, 5, 6)),         (5, 6, 7), 'infix:<«+«> left-dwim scalar';
is-deeply infix:<«+»>((1, 2), (4, 5, 6)),    (5, 7, 7), 'infix:<«+»> both-dwim extend';

# &-sigil form
is-deeply &infix:<»+«>((1, 2, 3), (4, 5, 6)), (5, 7, 9), '&infix:<»+«>';
is-deeply &infix:<»+»>((1, 2, 3), 1),         (2, 3, 4), '&infix:<»+»>';
is-deeply &infix:<«+«>(1, (4, 5, 6)),         (5, 6, 7), '&infix:<«+«>';
is-deeply &infix:<«+»>((1, 2), (4, 5, 6)),    (5, 7, 7), '&infix:<«+»>';

# &[...] form
is-deeply &[»+«]((1, 2, 3), (4, 5, 6)), (5, 7, 9), '&[»+«]';
is-deeply &[»+»]((1, 2, 3), 1),         (2, 3, 4), '&[»+»]';
is-deeply &[«+«](1, (4, 5, 6)),         (5, 6, 7), '&[«+«]';

# prefix / postfix hyper function forms (gist comparison)
is prefix:<-«>((2, 3, 4)).gist,    '(-2 -3 -4)',        'prefix:<-«> as function';
is &postfix:<»i>((2, 3, 4)).gist,  '(0+2i 0+3i 0+4i)',  '&postfix:<»i> as function';
