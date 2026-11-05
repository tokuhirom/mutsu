use Test;

plan 6;

# Rakudo::Iterator.Empty / .OneValue, called by lizmat's modules (roundrobin-slip).
my $e = Rakudo::Iterator.Empty;
ok $e.pull-one =:= IterationEnd, 'Empty is exhausted at once';
nok $e.is-lazy, 'Empty is not lazy';
my $o = Rakudo::Iterator.OneValue(7);
is $o.pull-one, 7, 'OneValue yields its value';
ok $o.pull-one =:= IterationEnd, 'then it is exhausted';
is Seq.new(Rakudo::Iterator.Empty).elems, 0, 'a Seq over Empty has no elements';
is Seq.new(Rakudo::Iterator.OneValue(3)).list.raku, '(3,)', 'a Seq over OneValue';
