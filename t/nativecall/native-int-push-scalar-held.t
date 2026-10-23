use Test;

plan 4;

my $a := array[uint8].new;
$a.push(-1);
$a.push(300, 2);
is $a.join(','), '255,44,2', 'push wraps onto a scalar-held native array (file scope)';

sub f {
    my $b := array[uint8].new;
    $b.push(-1, 256);
    $b.append(257);
    $b
}
is f().join(','), '255,0,1', 'push and append wrap onto a scalar-held native array (routine)';

my uint8 @e;
@e.push(1, 300, 2);
is @e.join(','), '1,44,2', 'push wraps onto a sigiled native array';

my $c := array[int8].new;
$c.unshift(200);
is $c.join(','), '-56', 'unshift wraps onto a scalar-held int8 array';
