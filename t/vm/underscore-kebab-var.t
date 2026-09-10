use Test;

plan 4;

# Raku allows underscore variants of kebab-case identifiers
# $*EXECUTABLE_NAME should be equivalent to $*EXECUTABLE-NAME

is $*EXECUTABLE-NAME, 'mutsu', '$*EXECUTABLE-NAME works';
is $*EXECUTABLE_NAME, 'mutsu', '$*EXECUTABLE_NAME works (underscore variant)';
is $*EXECUTABLE_NAME, $*EXECUTABLE-NAME, 'underscore and kebab variants are equivalent';

# User-defined dynamic variables with kebab-case should also be accessible
# via underscore variant
my $*my-var = 42;
is $*my_var, 42, 'user-defined $*my-var accessible as $*my_var';
