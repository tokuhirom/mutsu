use Test;

plan 6;

# #13448: an enum's keys are terms, so a package stash lists them under their
# bare spelling, not as `$`-scalars.

module M { enum E <ea eb> }
is M::.keys.sort.List, <E ea eb>, 'a package stash lists an enum and its keys bare';
is M::E::.keys.sort.List, <ea eb>, "an enum's own stash lists its keys bare";
is M::<ea>.raku, 'E::ea', 'a key is reachable by its bare name';

module N {
    our $v = 1;
    our constant K = 2;
    enum F <fa fb>;
}
is N::.keys.sort.List, <$v F K fa fb>, 'scalars keep their sigil next to enum keys and constants';

{
    my module P { enum Color <red green> }
    is P::.keys.sort.List, <Color green red>, 'a lexical module behaves the same';
}

enum Top <ta tb>;
ok !(GLOBAL::.keys.grep(* eq '$ta')), 'a top-level enum key is not published as a scalar';
