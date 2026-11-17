use experimental :rakuast;
use Test;

# A setting term in the unit tree `$*CU` resolves as rakudo 2026.09's does
# (#14916): `pi`, `Empty` (`Term::Name`) and `True`, `Less` (`Term::Enum`) to a
# `Declaration::External::Setting` named by the term, whose value is the
# term's: `maybe-compile-time-value` answers it (compared by `.raku`) and `has-compile-time-value`
# is the `Int` 1. A hand-built term is not resolved.

plan 15;

my %seen;
CHECK {
    sub walk($n) {
        if $n ~~ RakuAST::Term::Name && $n.name.canonicalize eq 'pi' | 'Empty' | 'True' | 'Less' {
            %seen{$n.name.canonicalize} //= [$n.^name, $n.resolution.^name,
                $n.resolution.lexical-name, $n.maybe-compile-time-value.raku,
                $n.has-compile-time-value];
        }
        $n.visit-children(&walk);
    }
    walk($*CU.statement-list);
}
my @seen = %seen<pi Empty True Less>;

my @values = pi, Empty, True, Less;

is @seen[0][0], 'RakuAST::Term::Name', 'pi is a Term::Name';
is @seen[0][1], 'RakuAST::Declaration::External::Setting', 'pi resolves to the setting';
is @seen[0][2], 'pi', 'by its own name';
is @seen[0][3], '3.141592653589793e0', 'its compile-time value is pi';
ok @seen[0][4] === 1, 'has-compile-time-value is the Int 1';
is @seen[1][2], 'Empty', 'Empty resolves to the setting';
is @seen[1][3], 'Empty', 'its value is Empty';
is @seen[2][0], 'RakuAST::Term::Enum', 'True is a Term::Enum';
is @seen[2][1], 'RakuAST::Declaration::External::Setting', 'an enum value resolves to the setting';
is @seen[2][3], 'Bool::True', 'its value is True';
is @seen[3][3], 'Order::Less', 'Less is Order::Less';
ok @seen[3][4] === 1, 'an enum value has a compile-time value';

my $hand = RakuAST::Term::Name.new(RakuAST::Name.from-identifier('pi'));
nok $hand.has-compile-time-value, 'a hand-built term has none';
throws-like { $hand.maybe-compile-time-value }, Exception,
    message => /'This element has not been resolved.'/, 'and is unresolved';
is @values.elems, 3, 'the probed statement still runs (Empty flattens away)';
