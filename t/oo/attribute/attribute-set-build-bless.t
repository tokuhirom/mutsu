use Test;

# A MOP `Attribute.set_build` closure builds the attribute through `.bless`
# as it does through `.new` (#18463; Red's `%!___DIRTY_COLS_DATA___` is built
# as a SetHash, and Red's `Red::Model.new` reaches it through `nextwith` →
# `Mu.new` → `bless`).

plan 7;

my $how = Metamodel::ClassHOW.new_type(:name<C>);
my $set = Attribute.new(:name<%!d>, :package($how), :type(Any), :!has_accessor);
$set.set_build(sub (|) { SetHash.new });
$how.^add_attribute($set);
my $num = Attribute.new(:name<$!s>, :package($how), :type(Any), :!has_accessor);
$num.set_build(sub (|) { 42 });
$how.^add_attribute($num);
$how.^compose;

isa-ok $set.get_value($how.new), SetHash, 'new runs a %-attribute build closure';
is $num.get_value($how.new), 42, 'new runs a $-attribute build closure';
isa-ok $set.get_value($how.bless), SetHash, 'bless runs a %-attribute build closure';
is $num.get_value($how.bless), 42, 'bless runs a $-attribute build closure';

{
    my $o = $how.bless;
    $set.get_value($o).{$num}++;
    ok $set.get_value($o).keys.head === $num, 'the SetHash keeps an object key';
}

# A user `new` that delegates with `nextwith` lands in `bless` too.
role R { method new(*%pars) { nextwith |%pars } }
my $how2 = Metamodel::ClassHOW.new_type(:name<D>);
$how2.^add_role(R);
my $attr2 = Attribute.new(:name<%!d>, :package($how2), :type(Any), :!has_accessor);
$attr2.set_build(sub (|) { SetHash.new });
$how2.^add_attribute($attr2);
$how2.^compose;
isa-ok $attr2.get_value($how2.new), SetHash, 'new through nextwith runs the build closure';

# The closure sees the object under construction and the attribute type.
my $how3 = Metamodel::ClassHOW.new_type(:name<E>);
my @seen;
my $attr3 = Attribute.new(:name<$!x>, :package($how3), :type(Int), :!has_accessor);
$attr3.set_build(sub ($obj, $type) { @seen.push($obj.^name); 7 });
$how3.^add_attribute($attr3);
$how3.^compose;
my $e = $how3.bless;
is-deeply (@seen, $attr3.get_value($e)), (['E'], 7), 'the closure gets the instance';
