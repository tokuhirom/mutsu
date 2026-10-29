use Test;

plan 12;

for <Cool Numeric Real Stringy Callable> -> $name {
    my $type = ::($name);
    my $obj = $type.new;
    ok $obj.defined, "$name.new is a defined instance";
    is $obj.^name, $name, "$name.new is a $name";
}

is Cool.new.raku, 'Cool.new', 'Cool.new.raku round-trips';
ok Cool.new ~~ Cool, 'Cool.new smartmatches Cool';
