use Test;

# From the SeqSplitter distribution: a class that `does Sequence` and supplies
# its own `iterator` (method or `has $.iterator` accessor) answers the
# list-ish protocol through that iterator.

plan 8;

class It does Iterator {
    has $.n = 0;
    method pull-one { $!n < 3 ?? $!n++ !! IterationEnd }
}
class ViaAccessor does Sequence {
    has Iterator:D $.iterator is required;
}
class ViaMethod does Sequence {
    method iterator { It.new }
}
sub mk { ViaAccessor.bless(iterator => It.new) }

is mk().List.raku, (0, 1, 2).List.raku, '.List drains the accessor iterator';
is mk().list.elems, 3, '.list';
is mk().elems, 3, '.elems';
is mk().Str, '0 1 2', '.Str';
is "{mk()}", '0 1 2', 'interpolation';
ok mk() eq '0 1 2', 'infix eq stringifies through the iterator';
is ViaMethod.new.List.raku, (0, 1, 2).List.raku, 'method iterator';

is ViaMethod.new.Str, '0 1 2', 'method iterator .Str';
