use Test;

plan 1;

is (reverse gather for 1, 2, 3 { take $_ }).join(','), '3,2,1',
    'reverse materializes a finite gather sequence before reversing it';
