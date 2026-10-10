use lib 't/lib';
use Test;

# #12517: a closure that is an attribute default resolves routine names
# through the module that declared it, not through the script calling `.new`
# (which never imported them).
use AttrDefaultClosureHolder;

plan 4;

my $h = AttrDefaultClosureHolder.new;
is $h.q, 8, 'plain attribute default sees the module import';
is $h.run(3), 6, 'closure default called from a method of the class';
is $h.p.(5), 10, 'closure default called from the script';
is $h.r.()(6), 12, 'closure returned by a closure default';
