use Test;

# EERPG's Price is a Rat subclass whose constructor delegates with
# self.Rat::new(...).  This exercises both qualified constructor forms that
# exposed the universal Mu.new lookup before the native Rat fallback.

plan 4;

class Price is Rat {
    method new($value) {
        self.Rat::new($value)
    }
}

my $price = Price.new(42);
isa-ok $price, Price, 'qualified Rat.new from a type object keeps the subclass';
is $price, 42, 'qualified Rat.new preserves the value';

my $rebuilt = $price.Rat::new(43);
isa-ok $rebuilt, Price, 'qualified Rat.new from an instance keeps the subclass';
is $rebuilt, 43, 'qualified Rat.new from an instance preserves the value';
