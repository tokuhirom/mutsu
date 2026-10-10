use Test;

plan 8;

# A role mixed into a scalar object is itself the single element of the
# object's `.list`/`.List`/`.Array`/`.Seq` (#12591).
role Col { method column { "colmeth" } }
class C { has Str $.t; }

my $at = C.^attributes[0];
$at does Col;
is $at.list[0].column, "colmeth", '.list keeps the mixin on an Attribute';
is $at.list.elems, 1, '.list of a scalar mixin has one element';
is $at.Seq[0].column, "colmeth", '.Seq keeps the mixin';

my $i = 5 but Col;
is $i.list[0].column, "colmeth", '.list keeps the mixin on an Int';
is $i.List[0].column, "colmeth", '.List keeps the mixin on an Int';
is $i.Array[0].column, "colmeth", '.Array keeps the mixin on an Int';

# An aggregate's mixin still folds into its elements.
my @a = 1, 2, 3;
my $m = @a but Col;
is $m.list.elems, 3, 'an Array mixin lists its elements';
is $m.list[0], 1, 'the elements are the inner ones';
