use Test;
plan 6;

# Test binding on `has $x` (attribute without twigil) aliases
{
    my $var = 42;
    my class Klass {
        has $x;
        method bind { $x := $var }
        method get_x { $x }
        method set_x ($new_x) { $x = $new_x }
    }

    my $obj = Klass.new;
    $obj.bind();

    is $obj.get_x, 42, 'binding has $x attribute reads bound value';
    $var = 23;
    is $obj.get_x, 23, 'binding has $x attribute tracks changes to source';
    $obj.set_x(19);
    is $var, 19, 'binding has $x attribute propagates writes back to source';
}

# Test binding on `has $.x` ($!x) instance attribute
{
    my $var = 100;
    my class Klass2 { has $.x; method bind { $!x := $var } }

    my $obj = Klass2.new;
    lives-ok { $obj.bind() }, 'binding $!x instance attribute lives';
    is $obj.x, 100, 'binding $!x reads bound value';
    $var = 200;
    is $obj.x, 200, 'binding $!x tracks source changes';
}
