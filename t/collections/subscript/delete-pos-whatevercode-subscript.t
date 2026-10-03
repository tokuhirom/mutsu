use Test;

# `$obj[*-N]:delete` on a user Positional class resolves the WhateverCode
# against `.elems` and calls DELETE-POS with the computed index, as rakudo
# does. It used to call DELETE-KEY with an empty key. Found in POFile, whose
# `$po[*-10]:delete` must throw its own POFile::IncorrectIndex.

plan 4;

class Store {
    has @.items = <a b c>;
    has @.log;
    method elems { @!items.elems }
    method AT-POS($i) { @!items[$i] }
    method DELETE-POS($i) { @!log.push("POS:$i"); "deleted $i" }
    method DELETE-KEY($k) { @!log.push("KEY:$k"); Nil }
}

my $s = Store.new;
is ($s[*-1]:delete), 'deleted 2', '*-1 deletes the last index';
$s[*-10]:delete;
$s[0]:delete;
$s<k>:delete;
is $s.log.join(' '), 'POS:2 POS:-7 POS:0 KEY:k', 'each subscript reaches the right method';

class X::Idx is Exception { method message { 'bad index' } }
class Strict {
    method elems { 2 }
    method DELETE-POS($i) { die X::Idx.new if $i < 0; $i }
}
my $t = Strict.new;
throws-like { $t[*-10]:delete }, X::Idx, 'a negative computed index reaches DELETE-POS';
is ($t[*-1]:delete), 1, '*-1 on a two-element object';
