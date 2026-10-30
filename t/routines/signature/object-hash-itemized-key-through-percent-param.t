use Test;

plan 9;

# An itemized Array/List key is filed under one string whichever subscript
# path touches it, so an object hash handed to a `%` parameter (whose binding
# re-tags the hash) keeps finding its entries.
my $ast = [{:keyw<not>}, {:ident<print>}];
my %c{List};
%c{$ast} = 7;
is %c{$ast}:exists, True, 'the key is found before any call';

sub exists-in(%h, $key) { %h{$key}:exists }
is exists-in(%c, $ast), True, 'a `%` parameter finds the entry';
is %c{$ast}:exists, True, 'the caller still finds it afterwards';

sub noop(%h) { 1 }
noop(%c);
is %c{$ast}:exists, True, 'binding the hash to a `%` parameter does not lose the entry';
is %c.elems, 1, 'and does not duplicate it';

sub put-in(%h, $key, $value) { %h{$key} = $value }
put-in(%c, $ast, 8);
is %c.elems, 1, 'storing the same key through a `%` parameter overwrites it';
is %c{$ast}, 8, 'with the new value';

sub read-from(%h, $key) { %h{$key} }
is read-from(%c, $ast), 8, 'reading through a `%` parameter';

sub delete-from(%h, $key) { %h{$key}:delete }
delete-from(%c, $ast);
is %c.elems, 0, 'deleting through a `%` parameter';
