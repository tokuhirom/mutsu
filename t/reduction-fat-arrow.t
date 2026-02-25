use Test;

plan 3;

my $pair = [=>] <A B C>;
is $pair.key, "A", "[=>] uses right reduction for keys";
ok $pair.value.key eq "B", "middle node is nested pair";
ok $pair.value.value eq "C", "tail value preserved";
