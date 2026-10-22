use Test;

# #14421: a lazy `.hyper` / `.race` pipeline runs its map/grep stages on the
# worker pool, `degree x batch` source elements at a time; a settled one honours
# `:degree`.
plan 7;
my $main = $*THREAD.id;
my @ids;
my $lock = Lock.new;
my @r = (1..*).map(*+0).hyper(:batch(1), :degree(4)).map({ $lock.protect({ @ids.push($*THREAD.id) }); sleep 0.05; $_ * 2 }).head(8).List;
is @r.join(','), '2,4,6,8,10,12,14,16', 'lazy hyper results stay in source order';
ok @ids.unique.elems > 1, 'the stages ran on more than one thread';
ok $main !~~ any(@ids), 'and not on the calling thread';

my $pulled = 0;
my @h = (1..*).map({ $lock.protect({ $pulled++ }); $_ }).hyper(:batch(4), :degree(2)).map(* + 1).head(3).List;
is @h.join(','), '2,3,4', 'a bounded lazy hyper gives the head';
ok $pulled <= 8, "the source is pulled at most degree x batch ahead (pulled $pulled)";

my $cur = 0; my $max = 0;
(1..8).hyper(:batch(1), :degree(2)).map({ $lock.protect({ $cur++; $max = $cur if $cur > $max }); sleep 0.05; $lock.protect({ $cur-- }); $_ }).List;
is $max, 2, 'a settled hyper honours :degree';

my @g = (1..*).map(*+0).race(:batch(2), :degree(2)).grep(* %% 2).head(3).sort.List;
is @g.join(','), '2,4,6', 'lazy race grep';
