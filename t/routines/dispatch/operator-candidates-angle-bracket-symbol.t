use Test;

# `.candidates` of a core operator whose symbol holds `<` or `>`
# (`&infix:«>»`, `&infix:«<=»`, `&infix:«=>»`) lists its candidates like any
# other operator's. Rakudo names such an operator with French quotes, and the
# candidate table keeps that spelling, so the `<op>` lookup found nothing and
# the `static` distribution's analyzer judged every `$x > 0` a mismatch.

plan 6;

ok &infix:«>».candidates.elems > 1, '&infix:«>» lists its candidates';
ok &infix:«<».candidates.elems > 1, '&infix:«<» too';
ok &infix:«<=».candidates.elems > 1, '&infix:«<=» too';
ok &infix:«>=».candidates.elems > 1, '&infix:«>=» too';
ok &infix:«>».candidates.grep(*.signature.params.elems == 2).elems > 1,
    'its candidates carry their signatures';
is &infix:«>».candidates.elems, &infix:«<».candidates.elems,
    '> and < have the same candidate count';
