use Test;
use lib $?FILE.IO.parent.parent.add('lib-unit-lexical-type-bareword');
use UnitLexicalTypeBareword;

# A module's unit-level `my class` read as a bareword, many times over, from
# the module's own subs: the per-site bareword memo now keeps it (it used to
# resolve through the whole chain on every read), and the memo must not leak
# into a block-scoped class of the same spelling.

plan 6;

my @answers;
for ^200 {
    @answers.push: found-or-not(make-not-found($_));
    @answers.push: found-or-not($_);
}
is @answers.grep('not-found').elems, 200, 'every boxed value is the lexical class';
is @answers.grep('found').elems, 200, 'every plain Int is not';
is name-of-not-found(), 'NotFound', 'the bareword names the lexical class';
is (inner-shadow() xx 3).join(','), 'inner,inner,inner', 'a block-scoped class of the same name wins inside its block';
is name-of-not-found(), 'NotFound', 'and the unit-level one still answers outside it';
is make-not-found(5) + 1, 6, 'the lexical class is still an Int';
