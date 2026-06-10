use Test;

# A constant declared with a `?` twigil (compiler variable) is not implemented;
# Raku rejects it at compile time with X::Comp::NYI.

plan 5;

throws-like 'constant $?FILE = "foo"', X::Comp::NYI, 'scalar ? twigil constant';
throws-like 'constant $?LINE = 1', X::Comp::NYI, 'another ? twigil constant';
throws-like 'constant @?FOO = 1, 2', X::Comp::NYI, 'array ? twigil constant';

# Ordinary constants are unaffected.
lives-ok {
    constant $pi = 3;
    constant NAME = "ok";
    constant @list = 1, 2, 3;
    die unless $pi == 3 && NAME eq "ok" && @list.elems == 3;
}, 'plain constants still work';

# The compiler variable itself is still usable as a term.
ok $?FILE.defined, '$?FILE is still a usable term';
