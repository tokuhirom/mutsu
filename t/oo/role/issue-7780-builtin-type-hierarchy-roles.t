use Test;

plan 17;

# These roles are part of the built-in type hierarchy, even though mutsu
# models their implementations natively rather than registering RoleDefs.
role QuantHashProbe does QuantHash {
    method marker { "QuantHash" }
}
role StringyProbe does Stringy {
    method marker { "Stringy" }
}
role BaggyProbe does Baggy {
    method marker { "Baggy" }
}
role PositionalBindFailoverProbe does PositionalBindFailover {
    method marker { "PositionalBindFailover" }
}
role BlobProbe does Blob[uint8] {
    method marker { "Blob" }
}

class SystemicProbe does Systemic {
    method marker { "Systemic" }
}
class QuantHashConsumer does QuantHashProbe { }
class StringyConsumer does StringyProbe { }
class BaggyConsumer does BaggyProbe { }
class PositionalBindFailoverConsumer does PositionalBindFailoverProbe { }
class BlobConsumer does BlobProbe { }

ok(Bag ~~ Baggy, "Bag composes Baggy");
ok(Bag ~~ QuantHash, "Bag composes QuantHash");
ok(Buf ~~ Blob, "Buf composes Blob");
ok(Str ~~ Stringy, "Str composes Stringy");
ok($*DISTRO ~~ Systemic, "Distro composes Systemic");

is(QuantHashConsumer.new.marker, "QuantHash", "a class receives a QuantHash role method");
ok(QuantHashConsumer.new ~~ QuantHash, "a class doing QuantHash type-checks as QuantHash");
is(StringyConsumer.new.marker, "Stringy", "a class receives a Stringy role method");
ok(StringyConsumer.new ~~ Stringy, "a class doing Stringy type-checks as Stringy");
is(BaggyConsumer.new.marker, "Baggy", "a class receives a Baggy role method");
ok(BaggyConsumer.new ~~ Baggy, "a class doing Baggy type-checks as Baggy");
is(
    PositionalBindFailoverConsumer.new.marker,
    "PositionalBindFailover",
    "a class receives a PositionalBindFailover role method",
);
ok(
    PositionalBindFailoverConsumer.new ~~ PositionalBindFailover,
    "a class doing PositionalBindFailover type-checks as PositionalBindFailover",
);
is(BlobConsumer.new.marker, "Blob", "a class receives a Blob role method");
ok(BlobConsumer.new ~~ Blob, "a class doing Blob[uint8] type-checks as Blob");
ok(BlobConsumer.new ~~ Stringy, "Blob[uint8] carries Stringy through its role hierarchy");
is(SystemicProbe.new.marker, "Systemic", "a class receives a Systemic role method");
