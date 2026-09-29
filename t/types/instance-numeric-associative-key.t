use Test;

plan 5;

class Keyed {
    method AT-KEY($key) { "key:$key" }
    method AT-POS($index) { "pos:$index" }
}

my $object = Keyed.new;
is $object{33}, 'key:33', 'integer brace subscript calls AT-KEY';
is $object{"a"}, 'key:a', 'string brace subscript still calls AT-KEY';
is $object[33], 'pos:33', 'integer bracket subscript still calls AT-POS';
is $object{1.5}, 'key:1.5', 'fractional brace key is not truncated';

class RejectingKey {
    method AT-KEY($key) { die "rejected $key" }
}
throws-like { RejectingKey.new{33} }, X::AdHoc,
    message => /'rejected 33'/,
    'an AT-KEY exception propagates through numeric brace indexing';
