use Test;

plan 5;

is ('Y', 'Z' ... 'A').join(' '), 'Y Z Y X W V U T S R Q P O N M L K J I H G F E D C B A',
    'two-seed nonnumeric sequence can reverse to reach endpoint';

throws-like { 'ZZ' ... 'A' },
    Exception,
    'string decrement underflow throws',
    message => 'Decrement out of range';

{
    my class H {
        has $.y = 5;
        method succ { H.new(y => $.y + 1) }
        method pred { H.new(y => $.y - 1) }
        method gist { $.y }
    }
    is (H.new ... *.y > 10).gist, '(5 6 7 8 9 10 11)', 'Seq.gist uses element gist';
}

is ('000' ... '077'), (0..0o77).fmt("%03o"), 'sequence supports octal-looking strings';
is ('077' ... '000'), (0..0o77).reverse.fmt("%03o"), 'sequence supports reverse octal-looking strings';
