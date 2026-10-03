use v6;
use Test;

my $json = Rakudo::Internals::JSON;

for '[', '{"a":' -> $open {
    my $close = $open eq '[' ?? ']' !! '}';
    my $document = ($open x 10000) ~ '0' ~ ($close x 10000);
    lives-ok { try $json.from-json($document) },
        "deeply nested $open input survives or raises a catchable error";
}

my $near-limit = ('[' x 200) ~ '0' ~ (']' x 200);
lives-ok { $json.from-json($near-limit) },
    'moderately nested JSON still parses';

my $value = 0;
for ^300 { $value = [$value] }
lives-ok { try $json.to-json($value, :!pretty) },
    'deeply nested arrays serialize or raise a catchable error';

my $object = 0;
for ^300 { $object = { a => $object } }
lives-ok { try $json.to-json($object, :!pretty) },
    'deeply nested objects serialize or raise a catchable error';

done-testing;
