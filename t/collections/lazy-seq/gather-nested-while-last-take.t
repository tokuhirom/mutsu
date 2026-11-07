use Test;

plan 3;

# A lazy gather with a `while` nested in a `while`, pulled through .map,
# must not lose the final take (#17019).
my @c = (gather {
    my @q = 1, 2;
    my $d;
    while @q {
        $d = @q.shift;
        my $k = 0;
        while $k < 2 {
            take $d * 100 + $k;
            $k++;
        }
    }
}).map({ $_ + 1 });
is @c.raku, '[101, 102, 201, 202]', 'while in while, through map';

my @e = (gather {
    my $i = 0;
    while $i < 2 {
        my $k = 0;
        loop (; $k < 3; $k++) {
            take $i * 10 + $k;
        }
        $i++;
    }
}).map({ $_ + 1 });
is @e.raku, '[1, 2, 3, 11, 12, 13]', 'loop in while, through map';

my @f = (gather {
    my $i = 0;
    while $i < 2 {
        my $k = 0;
        until $k >= 2 {
            take $i * 10 + $k;
            $k++;
        }
        $i++;
    }
}).map({ $_ + 1 });
is @f.raku, '[1, 2, 11, 12]', 'until in while, through map';
