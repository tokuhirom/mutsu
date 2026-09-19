use Test;

plan 2;

# A `method ... is export` registers an importable sub-form under an
# arity-suffixed registry key (`Class::name/arity`), mirroring the key shape
# genuine `multi sub`/`multi method` candidates use so `import` can find it
# by prefix scan. That shape is an implementation detail, not a semantic
# multi family: a plain, unrelated `sub` of the same name declared elsewhere
# in the same class body is a different namespace in Raku (confirmed against
# rakudo) and must not be rejected as "Redeclaration of routine" just
# because its key happens to collide with the forwarder's.
#
# Found via ML::AssociationRuleLearning's vendored ML::TriesWithFrequencies
# module (https://github.com/tokuhirom/mutsu/issues/8777), whose Trie class
# declares exactly this shape for `leafQ`.

class TrieLike {
    method leafQ(--> Bool) is export {
        return True;
    }
    sub leafQ(TrieLike $t --> Bool) {
        return $t.leafQ;
    }
}

ok(1, 'a class with an is-export method and a same-named sibling sub loads without a false redeclaration error');

my $t = TrieLike.new;
ok($t.leafQ, 'the exported method is still callable after the sibling sub declaration');
