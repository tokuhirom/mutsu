use Test;

plan 1;

role DefaultedRole[::ValueType = Any] {
    has ValueType $.value;
    submethod BUILD(ValueType() :$value) {
        $!value = $value;
    }
    method from-value(Str $value) {
        self.new(:$value);
    }
}

# Materialize the bare role first, leaving its default binding registered.
DefaultedRole.new(value => 'text');

class IntValue does DefaultedRole[Int] { }
is IntValue.from-value('3').value, 3,
    'a composed class keeps its concrete role binding after a bare role pun';
