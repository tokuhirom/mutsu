use Test;

plan 3;

my class Box {
    has Any:D $.value is required;
    submethod BUILD(::?CLASS:D:) {
        $!value
    }
}

throws-like { Box.new }, X::Attribute::Required, name => '$!value',
    'required attribute error is preserved with ::?CLASS:D: BUILD signature';

lives-ok { Box.new(value => 42) },
    'constructor works when required value is provided';

is Box.new(value => 42).value, 42, 'public accessor returns provided value';
