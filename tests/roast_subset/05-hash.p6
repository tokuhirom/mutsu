use Test;
plan 10;

# Basic hash declaration
my %h;
ok 1, 'hash declaration';

# Hash literal creation
my %colors = { red => 1, green => 2, blue => 3 };
ok 1, 'hash literal assignment';

# Hash access with angle brackets
is %colors<red>, 1, 'hash access with angle brackets';
is %colors<green>, 2, 'hash access - green';
is %colors<blue>, 3, 'hash access - blue';

# Hash access with curly braces
is %colors{'red'}, 1, 'hash access with curly braces';

# Non-existent key returns Nil
my $missing = %colors<yellow>;
ok !$missing.defined, 'non-existent key returns undefined';

# Boolean keys from colon pairs
my %flags = { :verbose, :debug };
is %flags<verbose>, True, 'colon pair - verbose is True';
is %flags<debug>, True, 'colon pair - debug is True';

# Empty hash is falsy
my %empty;
ok !%empty, 'empty hash is falsy';
