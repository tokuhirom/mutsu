use Test;

plan 6;

class A is IO::Path { }

my $p = A.new("foo");
isa-ok $p, A, "user IO::Path subclass .new returns subclass instance";
ok $p.isa(IO::Path), "user IO::Path subclass keeps IO::Path inheritance";
is $p.Str, "foo", "IO::Path native Str works on subclass instance";
is $p.basename, "foo", "IO::Path native basename works on subclass instance";

my $q = IO::Path::QNX.new("foo", :SPEC(my class Fail { }));
isa-ok $q, IO::Path::QNX, "platform IO::Path subclass .new returns subclass instance";
isa-ok $q.SPEC, IO::Spec::QNX, "platform IO::Path subclass forces matching IO::Spec";
