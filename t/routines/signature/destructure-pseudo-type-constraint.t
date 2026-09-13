use Test;

# `::?CLASS` / `::?ROLE` in a parameter may open a destructure or a coercion
# just like any other type name: `method m(::?CLASS:D (:$a, |))`.
#
# The parameter parser handles the pseudo-types in a branch of their own, which
# it entered before the general type-constraint path could see them. That branch
# recognized a variable (`::?CLASS $x`), an invocant marker (`::?CLASS:D:`) and
# a bare anonymous parameter (`multi prefix:<-->(::?CLASS)`) — and read anything
# else, `(` included, as the bare form, leaving the `(` unparsed and failing the
# enclosing signature. From ASTQuery (#7988).

plan 10;

class Holder {
    has $.a;
    has @.list;
    method by-attr(::?CLASS:D (:$a, |)) { $a }
    method untyped-smiley(::?CLASS (:$a, |)) { $a }
    method with-array(::?CLASS:D (:@list, |)) { @list.join('-') }
    method coerced(::?CLASS() $x) { $x.a }
    method plain(::?CLASS:D $o) { $o.a }
    method anon(::?CLASS) { 'anon' }
}

is Holder.by-attr(Holder.new(a => 7)), 7, 'method m(::?CLASS:D (:$a, |))';
is Holder.untyped-smiley(Holder.new(a => 8)), 8, 'method m(::?CLASS (:$a, |)) — no smiley';
is Holder.with-array(Holder.new(list => [1, 2, 3])), '1-2-3',
    'an @-sigil sub-parameter inside the destructure';
is Holder.coerced(Holder.new(a => 9)), 9, '::?CLASS() is still read as a coercion';

# The spellings the pseudo-type branch already handled must keep working.
is Holder.plain(Holder.new(a => 3)), 3, '::?CLASS:D $o still binds a plain parameter';
is Holder.anon(Holder.new), 'anon', 'a bare ::?CLASS is still one anonymous parameter';

role Shaped {
    method pair-up(::?ROLE:D (:$a, |)) { $a }
}
class Shapely does Shaped {
    has $.a;
}
is Shapely.pair-up(Shapely.new(a => 4)), 4, '::?ROLE:D (:$a, |) destructures too';

# An invocant marker still wins, and the whitespace-separated spelling keeps
# its invocant reading.
class Invocant {
    has $.v;
    method attached(::?CLASS:D:) { self.v }
    method spaced(::?CLASS:D : $n) { self.v + $n }
}
is Invocant.new(v => 5).attached, 5, '::?CLASS:D: is still an invocant marker';
is Invocant.new(v => 5).spaced(2), 7, '::?CLASS:D : $n is still an invocant marker';

# A bare pseudo-type parameter must not be read as an invocant — this is the
# CRDT spelling that motivated the bare-parameter branch.
class Counter {
    has $.n is rw;
}
multi prefix:<-->(Counter $c) { $c.n-- ; $c }
my $counter = Counter.new(n => 4);
is (--$counter).n, 3,
    'a bare type parameter on a prefix multi still binds one argument';
