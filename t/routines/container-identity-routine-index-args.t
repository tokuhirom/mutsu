use Test;

my @itemized = $(3), $(3);
my @plain = 3, 3;
sub same(\a, \b) { a =:= b }
my &meta = &[=:=];
my &named = &infix:<=:=>;

for &same, &meta, &named -> &compare {
    nok compare(@itemized[0], @itemized[1]), 'distinct itemized elements';
    nok compare(@plain[0], @plain[1]), 'distinct plain elements';
    ok compare(@plain[0], @plain[0]), 'same element twice';
}

my $alias := @plain[0];
ok meta($alias, @plain[0]), 'bound variable and its element share a container';
nok meta(@plain[0], 3), 'element and literal have distinct containers';
nok meta(3, @plain[0]), 'literal and element have distinct containers';
ok meta(3, 3), 'the same unboxed literal value is identical';

my $callable = &[=:=];
nok $callable(@plain[0], @plain[1]), 'callable value preserves distinct elements';
ok $callable(@plain[0], @plain[0]), 'callable value preserves the same element';

sub apply(&op, \a, \b) { op(a, b) }
nok apply(&[=:=], @plain[0], @plain[1]), 'raw parameters preserve distinct elements';
ok apply(&[=:=], @plain[0], @plain[0]), 'raw parameters preserve one element';

my &different = &infix:<!=:=>;
ok different(@plain[0], @plain[1]), 'negated identity sees distinct elements';
nok different(@plain[0], @plain[0]), 'negated identity sees the same element';

done-testing;
