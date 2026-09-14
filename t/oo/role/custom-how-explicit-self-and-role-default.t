use Test;

# A custom HOW method receives the type object as an explicit `$self` while
# its own implicit invocant remains the HOW instance. Role attribute defaults
# evaluated from that method must likewise see the value being composed.
role DefaultNameRole {
    has $.seen = self.^name;
}

class InspectHOW is Metamodel::ClassHOW {
    method inspect(Mu:U $self) {
        my $mixed = $self.new but DefaultNameRole;
        (self.^name, $self.^name, $mixed.seen)
    }

    # The slurpy forces the general method binder, which is the path used by
    # Red's multi methods with their richer signatures.
    method inspect-slow(Mu:U $self, *@rest) {
        my $mixed = $self.new but DefaultNameRole;
        (self.^name, $self.^name, $mixed.seen, @rest.elems)
    }
}

my $type = InspectHOW.new_type(name => 'MopExplicitSelf');
$type.^compose;
my $result = $type.^inspect;

is $result[0], 'InspectHOW', 'custom HOW method keeps its implicit invocant';
is $result[1], 'MopExplicitSelf', 'explicit $self receives the type object';
is $result[2], 'MopExplicitSelf+{DefaultNameRole}',
    'role default evaluates with the value being composed';

my $slow-result = $type.^inspect-slow(42, 43);
is-deeply $slow-result,
    ('InspectHOW', 'MopExplicitSelf', 'MopExplicitSelf+{DefaultNameRole}', 2),
    'the general method binder keeps explicit $self separate too';

done-testing;
