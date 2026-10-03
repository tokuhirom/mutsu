use v6;
use lib 't/lib';
use Test;

plan 1;

# #8083: a role method whose parameter names a GENUINELY mistyped type must
# not be silently accepted just because the role body `use`s a module that
# might have supplied it. The role body's `use` is BEGIN-time, so it has run
# by the time the method is declared, and the typo is reported when the role
# is declared -- i.e. when its module loads, as rakudo reports it ("Invalid
# typename 'TotallyBogusTypeName:U' in parameter declaration").

throws-like
    { EVAL q[use RolePendingTypo::R] },
    Exception,
    message => /'Invalid typename \'TotallyBogusTypeName:U\' in parameter declaration'/,
    'a genuinely mistyped role-method param type is caught once the role body\'s use has run';
