use Test;
use NativeCall;

# LLM::RetrievalAugmentedGeneration: `multi method nearest($vec where $vec ~~ Positional:D
# && $vec.all ~~ Numeric:D, ...)` rejected a `CArray[num64]`, because `.all` wrapped the
# object itself (`all(CArray.new)`) instead of its elements. `Any.any/all/one/none` are
# the junction of `self.list`, so a `Positional` object contributes its `.list`.

plan 10;

my $c = CArray[num64].new(1e0, 2e0, 3e0);
is $c.all.raku, 'all(1e0, 2e0, 3e0)', 'CArray.all is a junction of its elements';
is $c.any.raku, 'any(1e0, 2e0, 3e0)', 'CArray.any';
is $c.one.raku, 'one(1e0, 2e0, 3e0)', 'CArray.one';
is $c.none.raku, 'none(1e0, 2e0, 3e0)', 'CArray.none';
ok $c.all ~~ Numeric:D, 'every element is a defined Numeric';

sub takes($vec where $vec ~~ Positional:D && $vec.all ~~ Numeric:D) { 'ok' }
is takes($c), 'ok', 'a CArray passes the where clause that tests .all';

class P does Positional {
    method list { (1, 2, 3) }
    method elems { 3 }
}
is P.new.all.raku, 'all(1, 2, 3)', 'a user Positional class contributes its .list';

class Plain { has $.x = 1 }
is Plain.new.all.elems, 1, 'a plain object is still one item';
is (1, 2, 3).all.raku, 'all(1, 2, 3)', 'a List is unchanged';
is Int.all.raku, 'all(Int)', 'a type object is still one item';
