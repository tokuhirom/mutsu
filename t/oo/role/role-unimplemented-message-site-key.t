use Test;

# The "must be implemented by" message names the lexical class and the role as
# the program sees them, without a declaration-site key (#14540; found via
# AttrX::Mooish t/020-role.custom.rakutest).

plan 2;

role FooRole1 { method filter-bar { ... } }

my $msg = '';
try { EVAL 'my class FooR2 does FooRole1 { }'; CATCH { default { $msg = .message } } }
is $msg, "Method 'filter-bar' must be implemented by FooR2 because it is required by roles: FooRole1.",
    'lexical class and role are named without site keys';

my $msg2 = '';
try { EVAL 'class FooR3 does FooRole1 { }'; CATCH { default { $msg2 = .message } } }
is $msg2, "Method 'filter-bar' must be implemented by FooR3 because it is required by roles: FooRole1.",
    'package class too';
