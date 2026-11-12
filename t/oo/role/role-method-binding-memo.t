use Test;

# A role method sees its own composition's type parameter, call after call,
# whichever class composed the role (#17331 memoizes the bindings per
# (owner, receiver)).

plan 8;

role Box[::T = Any] {
    has $.value;
    method kind(--> Mu) { T }
    method wrap($v) { self.new(:value($v)) }
}

class IntBox does Box[Int] { }
class StrBox does Box[Str] { }

for ^2 {
    is IntBox.new.kind.^name, 'Int', 'IntBox sees T = Int';
    is StrBox.new.kind.^name, 'Str', 'StrBox sees T = Str';
}

is Box.new.kind.^name, 'Any', 'the pun sees the default T';

my $ib = IntBox.new(:value(1));
is $ib.wrap(5).kind.^name, 'Int', 'a method called from a role method keeps T';

# A class composed after the first calls gets its own bindings.
class NumBox does Box[Num] { }
is NumBox.new.kind.^name, 'Num', 'a later composition sees its own T';
is IntBox.new.kind.^name, 'Int', 'and the earlier one is unchanged';
