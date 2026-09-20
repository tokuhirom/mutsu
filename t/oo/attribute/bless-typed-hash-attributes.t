use Test;

plan 2;

class BlessTypedHashAttributesTest {
    has Str %.values is required;

    method new(*%args) {
        self.bless(|%args)
    }
}

my %source{Any};
%source<user> = 'testuser';
my $object = BlessTypedHashAttributesTest.new(:values(%source));
is $object.values<user>, 'testuser', 'bless preserves supplied hash values';
is $object.values.keys.head, 'user', 'bless converts object-hash keys for a plain hash attribute';
