use Test;

plan 2;

module RelativeMethodReturnTypeTest {
    role Packet::Base { }
    class Packet::Message does Packet::Base {
        method read(--> Packet::Base) { self }
    }

    role OpenPacket::Base { }
    class OpenPacket::Message does OpenPacket::Base {
        method read(--> OpenPacket::Base) { self }
    }
}

isa-ok RelativeMethodReturnTypeTest::Packet::Message.new.read,
    RelativeMethodReturnTypeTest::Packet::Base,
    'a relative qualified return type resolves against its owner package';
isa-ok RelativeMethodReturnTypeTest::OpenPacket::Message.new.read,
    RelativeMethodReturnTypeTest::OpenPacket::Base,
    'a second package with the same base name keeps its own return type';
