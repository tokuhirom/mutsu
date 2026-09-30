use Test;

# A role mixed into an instance (`Foo.new but role { ... }`, as zef's plugin
# loader does) keeps the wrapped class's accessors, including ones spelled
# like an `Any` method: `$.cache` must not answer as `Any.cache` (#10232).

plan 8;

class Holder {
    has $.cache;
    has $.list;
    has $.elems;
    method via-self { self.cache }
    method via-dollar { $.cache }
}

my $h = Holder.new(:cache<c>, :list<l>, :elems<e>) but role { has $.short-name = 'n' };

is $h.cache, 'c', 'cache accessor on a named receiver';
is $h.list, 'l', 'list accessor';
is $h.elems, 'e', 'elems accessor';
is $h.short-name, 'n', 'the role attribute is still reachable';
is $h.via-self, 'c', 'self.cache inside a class method';
is $h.via-dollar, 'c', '$.cache inside a class method';
is (Holder.new(:cache<d>) but role { }).cache, 'd', 'cache accessor on an unnamed receiver';
is-deeply (Holder.new but role { }).^name.starts-with('Holder+'), True, 'still a mixin';
