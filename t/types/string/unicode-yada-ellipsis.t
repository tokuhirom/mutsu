use Test;

plan 2;

lives-ok {
    EVAL q[sub unicode-stub { … }];
}, 'unicode ellipsis parses as a stub term';

lives-ok {
    EVAL q[
        sub unicode-do-whenever($s) {
            react {
                my $tap = do whenever $s -> $v { … };
                done;
            }
        }
    ];
}, 'unicode ellipsis parses inside do whenever block';
