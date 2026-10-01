use Test;

plan 4;

{
    use attributes :D;
}
class Outside { has Int $.x }
is Outside.new.x.raku, 'Int', 'use attributes :D ends with its block';

{
    use attributes :D;
    {
        use attributes :_;
    }
}
class Outside2 { has Int $.x }
is Outside2.new.x.raku, 'Int', 'a nested block pragma does not outlive the outer block either';

sub f() { use attributes :U; 1 }
f();
class Outside3 { has Int $.x }
is Outside3.new.x.raku, 'Int', 'a pragma in a routine body does not leak';

{
    use attributes :D;
    class Inside { has Int $.x = 7 }
    is Inside.new.x, 7, 'a :D attribute with an initializer works inside its block';
}
