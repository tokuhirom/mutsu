use Test;

plan 5;

is (1, 2).map({ when Int { "int" } }).join(','), 'int,int',
    'map: a matched when inside the block no longer aborts the map';

is (1, 2).grep({ when Int { True } }).join(','), '1,2',
    'grep: a matched when inside the block no longer aborts the grep';

is (1, "a").map({ when Int { "int" }; default { "other" } }).join(','), 'int,other',
    'map: default absorbs the succeed signal too';

{
    my @a = (1, 2);
    @a .= map({ when Int { $_ * 10 } });
    is @a.join(','), '10,20', 'rw map (.=map) absorbs the succeed signal';
}

class Part {
    has $.value;
}
sub serialize(@parts) returns Supply {
    supply {
        for @parts.map({ when Part { .value } }) -> $v {
            emit $v;
        }
    }
}
is serialize((Part.new(:value(1)), Part.new(:value(2)))).list.join(','), '1,2',
    'the --> Supply method shape (Cro BodySerializer pattern) absorbs the succeed signal';

done-testing;
