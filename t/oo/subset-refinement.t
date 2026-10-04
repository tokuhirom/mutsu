use Test;

plan 9;

subset S of Int where * > 3;
subset T of Int;
subset R of Str where /a/;
subset B of Int where { $_ %% 2 };

is UInt.^refinement.^name, 'Block', 'UInt.^refinement is a Block';
ok UInt.^refinement.(0), 'UInt refinement accepts 0';
nok UInt.^refinement.(-1), 'UInt refinement rejects -1';
is S.^refinement.^name, 'WhateverCode', 'WhateverCode predicate';
ok S.^refinement.(5), 'S refinement accepts 5';
nok S.^refinement.(2), 'S refinement rejects 2';
is T.^refinement.raku, 'Mu', 'subset without where answers Mu';
is R.^refinement.^name, 'Block', 'non-code predicate is wrapped as a Block';
ok B.^refinement.(4) && !B.^refinement.(3), 'block predicate is callable';
