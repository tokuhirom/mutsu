use Test;

# A gather's lazy List passed as a named argument is not run to completion
# before the call; the callee pulls what it reads (LibTmux::Batch).

plan 6;

my $visited = 0;
my $deferred = (gather for ^1000 { ++$visited; take $_ }).List;

class B {
    method count-to(Mu :$items!, Int :$limit!) {
        my $it = $items.iterator;
        my $n = 0;
        until (my $v := $it.pull-one) =:= IterationEnd {
            last if ++$n >= $limit;
        }
        $n
    }
}
is B.count-to(:items($deferred), :limit(3)), 3, 'the method reads three elements';
ok $visited <= 4, 'only those elements were produced';

is $deferred.^name, 'List', 'a metamethod pulls nothing';
ok $visited <= 4, 'still only those elements';

class C { has @.c; has $.s; }
is C.new(c => (gather { take 1; take 2 }).List).c, [1, 2], 'an array attribute is filled';
is C.new(c => gather { take 3; take 4 }).c, [3, 4], 'a gather Seq still fills an attribute';
