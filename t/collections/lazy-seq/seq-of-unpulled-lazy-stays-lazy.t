use Test;
plan 4;

my @log;
my $m = (1, 2).map({ @log.push("m$_"); $_ });
my $ms = $m.Seq;
is @log.elems, 0, '.Seq of an unpulled map runs nothing';

my $g = gather { @log.push("g"); take 1 };
my $gs = $g.Seq;
is @log.elems, 0, '.Seq of an unpulled gather runs nothing';
is $gs.list.join(","), "1", 'the gather still yields when pulled';
is $ms.list.join(","), "1,2", 'the map still yields when pulled';
