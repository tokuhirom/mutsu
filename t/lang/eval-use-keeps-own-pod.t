use Test;

# An EVAL'd unit has its own `$=pod`; loading a module (`use Test`, `require`)
# inside it must hand that document back afterwards, not leave the module's.
plan 4;

use MONKEY-SEE-NO-EVAL;

is EVAL("use Test;\n=begin pod\n\nhi\n\n=end pod\n\$=pod[0].contents.elems"), 1,
    'use Test in an EVAL keeps the unit pod';
is EVAL("=begin pod\n\nhi\n\n=end pod\nrequire Test;\n\$=pod[0].contents.elems"), 1,
    'require in an EVAL keeps the unit pod';
is EVAL("use NativeCall;\n=begin pod\n\nhi\n\n=end pod\n\$=pod[0].contents.elems"), 1,
    'a module without pod does not clear the unit pod';
is EVAL("use Test;\n=begin pod\n\nhi\n\n=end pod\n\$=pod[0].gist.lines.elems"), 3,
    'the pod gist is the unit own, not the module';
