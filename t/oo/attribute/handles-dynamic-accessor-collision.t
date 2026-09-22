use Test;

plan 2;

role AddressRole {
    method street { Str }
}

role ActionableLike {
    multi method action(Any:U: $street, *%_) {
        self.new(:$street)
    }

    multi method action(Any:D: $street, *%_) {
        self
    }
}

class Address does AddressRole does ActionableLike {
    has Str $.street;
}

my $class = Address;
my $address = $class.action('Main Street');

is Address.action('Main Street').street, 'Main Street',
    'a class-level accessor wins over a role method for a direct action';
is $address.street, 'Main Street',
    'a delegated mutable read preserves class accessor precedence';
