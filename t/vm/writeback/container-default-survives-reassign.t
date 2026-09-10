use Test;

plan 8;

# A container's `is default(...)` element default is a property of the
# container bound to the variable. Raku `@array = ...` / `%hash = ...` assigns
# INTO that container, so the default survives the reassignment — including a
# `= Nil` reset (which clears the elements but keeps the container). Previously
# mutsu replaced the container with a fresh one on reassignment and lost the
# default, so a later out-of-range read returned Any instead of the default.

my @array is default( 'N/A' );
is @array[22], 'N/A', 'array default before any assignment';

@array = Nil;
is @array.raku, '["N/A"]', '= Nil resets the array (element takes the default)';
is @array[4], 'N/A', 'array default survives = Nil';

@array = 1, 2;
is @array[10], 'N/A', 'array default survives = LIST';

@array = ();
is @array[10], 'N/A', 'array default survives = ()';

my %hash is default( 'no-value-here' );
%hash<foo> = 'bar';
is %hash<wrong-key>, 'no-value-here', 'hash default before reassignment';

%hash = (a => 1);
is %hash<missing>, 'no-value-here', 'hash default survives = LIST';

# A container with no declared default is unaffected (out-of-range reads Any).
my @plain = 1, 2, 3;
@plain = 4, 5;
is @plain[9].raku, 'Any', 'no default declared -> out-of-range reads Any';
