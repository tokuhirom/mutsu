use Test;

plan 4;

# Net::BGP (ecosystem): a lowercase native type with a smiley as the return
# constraint (`--> buf8:D`) is a type, not a definite return value, so the body
# may `return` a value.
sub bytes-of(Int $x --> buf8:D) { return buf8.new($x) }
sub int-of(Int $x --> int:D) { return $x }
sub blob-of(Int $x --> Blob:D) { return Blob.new($x) }

is bytes-of(5).elems, 1, 'buf8:D return type';
is int-of(7), 7, 'int:D return type';
is blob-of(9).elems, 1, 'Blob:D return type';
throws-like { EVAL 'sub f(--> 5) { return 6 }' }, Exception, 'a literal return value still refuses a return argument';
