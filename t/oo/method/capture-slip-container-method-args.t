use Test;

plan 5;

# #12907: a spread capture keeps its positionals' containers; a built-in
# method reads through them.
my $b = "abcdef".encode("latin-1");
my $p = 1;
my $c = \($p, 2);
is $b.subbuf(|$c).list, (98, 99), 'Blob.subbuf over a spread capture with a container';
is (1, 2, 3, 4).head(|\($p)), (1,), 'List.head over a spread capture';

sub fwd(|c) { $b.subbuf(|c) }
is fwd($p, 3).list, (98, 99, 100), 'a `|c` forwarded to a built-in method';

class S { has $!o; method o { $!o //= "abcdef".encode("latin-1") }; method sb(|c) { $.o.subbuf(|c) } }
is S.new.sb($p, 1).list, (98,), 'a method forwarding its capture';

# A user method with an `is rw` parameter still gets the container.
class W { method set($x is rw) { $x = 42 } }
my $q = 0;
W.set(|\($q));
is $q, 42, 'is rw parameter writes through a spread capture';
