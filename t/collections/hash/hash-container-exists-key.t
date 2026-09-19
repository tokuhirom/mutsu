use Test;

# Regression reduced from Data::StaticTable 0.1.1's nested dimension lookup.
plan 2;

my %inner = '7' => 1;
my %outer = inner => %inner;

ok %outer<inner><7>:exists,
    ':exists decontainerizes a hash value from a chained lookup';
ok %outer<inner>{7}:exists,
    'braced chained lookup also finds a numeric key in a string-keyed hash';
