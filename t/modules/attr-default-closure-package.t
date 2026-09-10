use lib 't/lib';
use Test;
use AttrDefaultClosureScope;
use AttrDefaultClosureUser;

# An attribute default (`has $.x = -> { helper() }`) is lowered to bytecode when
# the class is declared but EXECUTED inside whatever frame calls `.new`. mutsu
# stamped the *constructing* frame's class onto any closure the default built,
# so the closure later ran under the wrong package and could not see its own
# class's file-scoped subs — but only when `.new` was called from inside another
# module's method, which is what made it look like a scoping mystery rather than
# a bug. Reduced from `Template6`, whose `%!directive-handlers` closures could
# not call `parse-set` once `Template6::Context::BUILD` was the constructor.

plan 10;

# Constructed from the mainline: this always worked.
my $direct = AttrDefaultClosureScope.new;
is $direct.via-attr(1), 'helper(1)', 'mainline: scalar attribute-default closure';
is $direct.via-table('wrapped', 2), 'helper(2)', 'mainline: hash-valued default closure';
is $direct.via-table('direct', 3), 'helper(3)', 'mainline: `&sub` term in a default';
is $direct.via-nested(4), 'helper(4)', 'mainline: closure returned by a default closure';

# Constructed from inside another module's method: this is what regressed.
my $via = AttrDefaultClosureUser.new;
is $via.attr(1), 'helper(1)',
   'cross-module: scalar attribute-default closure sees its own class scope';
is $via.table('wrapped', 2), 'helper(2)',
   'cross-module: hash-valued default closure sees its own class scope';
is $via.table('direct', 3), 'helper(3)',
   'cross-module: `&sub` term in a default still resolves';
is $via.nested(4), 'helper(4)',
   'cross-module: a closure returned by a default closure keeps the scope';
is $via.local(5), 'helper(5)',
   'cross-module: a closure built in a method body is unaffected';
is $via.direct(6), 'helper(6)',
   'cross-module: a plain call from a method body is unaffected';
