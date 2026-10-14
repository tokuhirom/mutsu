use Test;

# #13313: a buffer is not `Cool`, so it has none of `Cool`'s text methods;
# the by-name dispatchers used to stringify it (`Buf.new(1).contains(1)`
# was `True`). `chars`/`codes` are `Blob`'s own (they raise X::Buf::AsStr),
# and a buffer class's own method still answers.

plan 12;

my $b = Buf.new(1, 2, 3);
for <contains starts-with ends-with index rindex substr-eq> -> $m {
    throws-like { $b."$m"(Buf.new(1)) }, X::Method::NotFound, "Buf.$m";
}
throws-like { Blob.new(65).contains(1) }, X::Method::NotFound,
    message => "No such method 'contains' for invocant of type 'Blob'", 'Blob.contains';
throws-like { utf8.new(65).index("A") }, X::Method::NotFound, 'utf8.index';
throws-like { Buf.contains(1) }, X::Method::NotFound, 'the Buf type object';
throws-like { $b.chars }, X::Buf::AsStr, 'chars is the buffer own, X::Buf::AsStr';

class UB does Blob[uint8] is repr('VMArray') { method contains($) { 'mine' } }
is UB.new(1).contains(1), 'mine', 'a buffer class own method answers';

# `Cool` declares `substr-eq`, so `Any` (not `Cool`) has none.
my $u;
throws-like { $u.substr-eq("a") }, X::Method::NotFound, 'Any.substr-eq';
