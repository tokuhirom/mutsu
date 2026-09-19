use Test;

# Regression reduced from Data::StaticTable 0.1.1's row-header construction.
plan 1;

my @rows = (<a b>, <c d>);
my @header = @rows[0];
@header = @header>>[].flat;

is-deeply @header.List, <a b>,
    'an empty hyper subscript decontainerizes each selected row';
