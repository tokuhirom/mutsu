unit module EscapedOpNameFixture;

# Shape of List::Operator::DoublePlus: an `our proto` whose multis differ by
# type, aliased to operators and exported. The Unicode operator is spelled with
# an escape inside a quoted operator name.
our proto concat(|) {*}
multi sub concat(@a, @b --> List) { |@a, |@b }
multi sub concat(Array \a, Array \b --> Array) { a.append: b }

our &infix:<++> is export(:DEFAULT :op) = &concat;
our &infix:["\c[DOUBLE PLUS]"] is export(:DEFAULT :op) = &concat;
