use lib 't/lib';
use Test;
use SubsetModuleLexical;
use SubsetModuleLexicalPlain;

plan 5;

# A `subset ... where EXPR` whose value predicate reads a lexical of the
# declaring file keeps reading it when the type is checked from another unit
# (JSON::Fast::Hyper's `Hyperable`).
is fj("[\n\nabc"), 'hyper', 'the subset candidate of an exported family is chosen';
is fj("abc"), 'plain', 'a non-matching string takes the dispatchee';
is &fj.candidates.elems, 2, 'both candidates are listed';
ok chk("Habc"), 'a subset check from an exported routine';
nok chk("abc"), 'and its negative';
