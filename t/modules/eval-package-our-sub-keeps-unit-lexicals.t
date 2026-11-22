use Test;
use MONKEY-SEE-NO-EVAL;

# An EVAL run in a package, or one that opens its own with `unit module`,
# keeps its file-scope `my` lexicals for the `our sub`s it declares once the
# EVAL returns, and its `is export` subs stay in the package's EXPORT stash
# (#17944).

plan 16;

module QQ { EVAL q[my $pcs = 5; our sub f { $pcs }] }
is QQ::f(), 5, 'an EVAL in a package: the our sub reads the unit lexical';

EVAL q[unit module QQ2; my $pcs = 5; our sub f { $pcs }];
is QQ2::f(), 5, 'an EVAL with `unit module`: the our sub reads the unit lexical';

EVAL q[unit module CounterMod; my $n = 0; our sub inc { ++$n }];
CounterMod::inc();
is CounterMod::inc(), 2, 'a write through the unit lexical persists between calls';

module ArrMod { EVAL q[my @a = 1, 2; our sub g { @a.elems }] }
is ArrMod::g(), 2, 'an array unit lexical';

module Shadow {
    my $pcs = 1;
    our sub own { $pcs }
    EVAL q[my $pcs = 5; our sub f { $pcs }; our sub set($v) { $pcs = $v }]
}
is Shadow::f(), 5, 'the EVAL unit lexical, not the package block lexical of the same name';
is Shadow::own(), 1, '... and the package block sub still reads its own';
Shadow::set(9);
is Shadow::f(), 9, 'a write lands in the EVAL unit lexical';
is Shadow::own(), 1, '... not in the package block lexical';

EVAL q[unit module Same1; my $v = 1; our sub v { $v }];
EVAL q[unit module Same2; my $v = 2; our sub v { $v }];
is Same1::v(), 1, 'two EVAL units with a same-named sub: the first reads its own';
is Same2::v(), 2, '... and the second reads its own';

EVAL q[unit module ExpMod; sub g is export { 7 }];
is ExpMod::EXPORT::DEFAULT::<&g>(), 7, 'an `is export` sub of an EVAL unit stays in EXPORT::DEFAULT';
is ExpMod::EXPORT::ALL::<&g>(), 7, '... and in EXPORT::ALL';

EVAL q[module ExpBlock { sub tg is export(:t) { 8 } }];
is ExpBlock::EXPORT::t::<&tg>(), 8, 'a tagged export of a package block in an EVAL';

EVAL q[unit module ExpLex; sub h is export { 1 }];
nok (try EVAL q[h()]).defined, 'the exported sub is still not lexical to the caller';

# Package blocks inside one EVAL keep their own lexicals: they are not the
# unit's top level.
is EVAL(q[module BlkC { my $t = 'C'; our sub r { $t } }; module BlkD { my $t = 'D'; our sub r { $t } }; BlkC::r() ~ BlkD::r()]),
    'CD', 'two package blocks of one EVAL with a same-named `my`';
is EVAL(q[module OurC { our $t = 'C'; our sub r { $t } }; module OurD { our $t = 'D'; our sub r { $t } }; OurC::r() ~ OurD::r()]),
    'CD', 'two package blocks of one EVAL with a same-named `our`';
