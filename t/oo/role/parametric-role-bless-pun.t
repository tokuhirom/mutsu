use Test;

plan 5;

# A role's own `method new` calling `self.bless` on a parameterised role
# blesses the class the role puns to; blessing the bare role first must not
# leave a class behind that a later `R[...]` pun composes from.
role Ordered[$key = * cmp *] {
    has &.key = $key;
    has @.data;
    method new(+@data) { self.bless: :@data }
    method first-by { @!data.sort(&!key).head }
}

my $plain = Ordered.new(3, 1, 2);
is $plain.data, [3, 1, 2], 'bare role: bless sets the attributes';
ok $plain ~~ Ordered, 'bare role: the instance does the role';

my $neg = Ordered[-*].new(3, 1, 2);
is $neg.first-by, 3, 'parameterised role: bless uses its own argument';
is Ordered[* + 0].new(3, 1, 2).first-by, 1, 'a second parameterisation is distinct';
is Ordered[-*].new(5, 9).first-by, 9, 'a later parameterisation after the bare role';
