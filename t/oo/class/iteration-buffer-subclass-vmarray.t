use Test;
use nqp;

# ValueList / Tuple (zef distribution Tuple): a `my class ... is IterationBuffer
# is repr('VMArray')` that declares methods. `nqp::create(self)` must give a
# real instance (so `!SET-SELF` dispatches) backed by IterationBuffer storage,
# `(@a)` must beat `(+@a)` in a multi, and an `is Foo` array variable must
# survive being passed to a `\c` parameter.

plan 14;

my class VL is IterationBuffer does Positional does Iterable is repr('VMArray') {
    proto method new(|) {*}
    multi method new(VL: @args) { nqp::create(self)!SET-SELF: @args.iterator }
    multi method new(VL: +@args) { nqp::create(self)!SET-SELF: @args.iterator }
    method !SET-SELF(\iterator) is raw {
        nqp::until(
          nqp::eqaddr((my \pulled := iterator.pull-one), IterationEnd),
          nqp::push(self, nqp::decont(pulled))
        );
        self
    }
    multi method raku(VL:D:) { 'VL.new(' ~ self.List.map(*.raku).join(',') ~ ')' }
    multi method Str(VL:D:) { self.raku }
    method list() { self.List }
}

my $v = VL.new(1, 2, 3);
is $v.^name, 'VL', 'nqp::create(self) keeps the class';
is $v.elems, 3, '.elems counts the pushed elements';
is $v.List, (1, 2, 3), '.List';
is $v.raku, 'VL.new(1,2,3)', 'user multi method raku sees the elements';
is $v.Str, 'VL.new(1,2,3)', 'user multi method Str';
is $v.list, (1, 2, 3), 'user method list';
is $v[1], 2, 'positional access';
is $v.iterator.pull-one, 1, '.iterator walks the elements';
is VL.new(^4).elems, 4, 'a Range argument picks the (@args) candidate';
is VL.new([5, 6]).elems, 2, 'an Array argument picks the (@args) candidate';
is VL.new(7, 8, 9).elems, 3, 'plain arguments pick the (+@args) candidate';

my class Box does Positional {
    method STORE(\to_store, :$INITIALIZE) { self }
    method elems { 1 }
    method AT-POS($) { 1 }
    method iterator { ().iterator }
}
sub sigilless(\x) { 1 }
my @a is Box = 1, 2, 3;
sigilless(@a);
is @a.^name, 'Box', 'an `is Box` array passed to a sigilless param stays a Box';
sub slurpy(+@x) { 1 }
slurpy(@a);
is @a.^name, 'Box', 'and to a +@ param';
Box.new.STORE(@a, :INITIALIZE);
is @a.^name, 'Box', 'and to a method taking \\c';

