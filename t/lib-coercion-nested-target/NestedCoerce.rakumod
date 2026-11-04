# From the SQL::Abstract distribution: a class with a nested name
# (`Column::List`) declared inside a module, used as a coercion type in that
# module's own routine signatures.
unit module NestedCoerce;

class Column::List {
    has @.elements;
    multi method COERCE(Any $column) { self.new(:elements[$column]) }
    multi method COERCE(@list) { self.new(:elements(@list)) }
}

our sub one(Column::List(Any) $columns) is export(:functions) { $columns.elements.join(',') }
our sub with-from(Str $from, Column::List(Any) $columns) is export(:functions) {
    "$from:" ~ $columns.elements.join(',')
}
our sub kind(Column::List(Any) $columns) is export(:functions) { $columns.^name }
