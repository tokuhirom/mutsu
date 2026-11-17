unit module TdAlias::Shapes;

# A nested class whose short name the module's routines use; importing the
# module gives the importer a package alias table that is not empty.
class Point is export { has $.x = 1 }
class Str { method kind { 'nested Str' } }

our sub local-str { my Str $s = Str.new; $s.kind }
our sub plain-int($n) { my Int $total = 0; $total += $_ for ^$n; $total }
