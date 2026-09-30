# #10247: `require Q;` is a module named `Q`, not a `Q;...;` quote construct.
use lib 't/lib';
use Test;

plan 2;

require Q;
pass 'require Q; followed by a newline loads the module';
is ::("Q").go, 42, 'the module named Q is loaded';
