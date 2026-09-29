use Test;

# From the List::Agnostic distribution (t/01-basic.rakutest): a `:=` rebind of a
# file-scope lexical must reach the named subs and class methods that read it,
# whether the rebind is made by the mainline or from inside a method.

plan 8;

my $l;
sub read-l { $l }
$l := 5;
is read-l(), 5, 'mainline rebind is seen by a named sub';

my $k = 1;
class C { method get { $k } }
$k := (7, 8);
is C.new.get.raku, '(7, 8)', 'mainline rebind is seen by a class method';

my $list;
class Setter {
    method set(\v) { $list := v.List; self }
    method get(\pos) { $list[pos] }
    method all() { $list.raku }
}
Setter.new.set((1, 2, 4));
is $list.raku, '(1, 2, 4)', 'a method rebind reaches the declaring frame';
is Setter.new.get(1), 2, 'a method rebind is seen by a sibling method';
is Setter.new.all, '(1, 2, 4)', 'sibling method sees the whole rebound value';

# `my @x is Class = ...` calls the class's STORE; a rebind of an outer lexical
# done inside that user STORE must survive the call.
my $stored;
class Tied {
    method AT-POS(\pos) { $stored[pos] }
    method elems() { $stored.elems }
    method STORE(\values, :$INITIALIZE) {
        $INITIALIZE ?? ($stored := values.List) !! die "no";
        self
    }
}
my @m is Tied = 1, 2, 4;
is $stored.raku, '(1, 2, 4)', 'STORE rebinding an outer lexical reaches the caller';
is @m[1], 2, 'element read through the STOREd instance';

# `@x := $obj` accepts an object whose Positional comes from a composed role's role.
role Inner does Positional { }
role Outer does Inner { method AT-POS(\p) { 42 } }
class Holder does Outer { }
my $h = Holder.new;
my @h := $h;
is @h[0], 42, 'binding to @ accepts Positional reached through a nested role';
