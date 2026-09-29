use Test;
use MONKEY-SEE-NO-EVAL;

plan 4;

# A store can throw even though its opcode does not normally refresh the
# current source line. The backtrace must locate the failing instruction.
try EVAL "my Int \$d = 3;\nsay 1;\n\$d = \"x\";";
is $!.^name, 'X::TypeCheck::Assignment', 'the scalar assignment rejects Str';
ok $!.backtrace.Str.contains('line 3'),
    'the scalar assignment reports its own line';

try EVAL "my Int \@a = 1;\nsay 1;\n\@a[0] = \"x\";";
is $!.^name, 'X::TypeCheck::Assignment', 'the array element rejects Str';
ok $!.backtrace.Str.contains('line 3'),
    'the element assignment reports its own line';
