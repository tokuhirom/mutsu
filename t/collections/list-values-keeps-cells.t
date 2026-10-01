use Test;

plan 5;

# A List built from variables keeps their containers; `.values` hands the
# slots out as stored, so a loop over it aliases the variables.
my $a = 1; my $b = 2;
my @l := ($a, $b);
$_ = 9 for @l.values;
is "$a $b", "9 9", '.values of a List built from variables aliases them';

my @x = (1, 2).values;
is @x, [1, 2], '.values of a literal List still yields the values';

my Int @t = @l.values;
is @t, [9, 9], 'a typed array assigned from .values decontainerizes';

is (1, 2, 3).values.map(* + 1), (2, 3, 4), '.values.map works on bare items';

my @lit := (1, 2);
dies-ok { $_ = 5 for @lit.values },
    'a loop over .values of bound literal items stays read-only';
