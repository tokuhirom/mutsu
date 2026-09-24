use Test;

# Data::Tree 0.3 declares a lexical multi method at module scope.  Keep the
# declaration as a first-class Method and dispatch its registered candidate
# when the code value is called through `&name` or `.&name`.

plan 4;

my multi method from-data-tree(Int $value) { "value:$value" }

is &from-data-tree.WHAT.^name, 'Method', 'a lexical multi method remains a Method';
is &from-data-tree(Any, 7), 'value:7', 'the code value dispatches its candidate';
is 42.&from-data-tree(8), 'value:8', 'the dot-ampersand call dispatches its candidate';
is &from-data-tree.arity, 2, 'the method value includes its invocant';
