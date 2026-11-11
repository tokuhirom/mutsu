use Test;

# `callframe(1).code` seen from a routine called by the mainline is the unit's
# `Block`, named `<unit>`, starting at line 1 of the program's file -- as in
# Rakudo. REPL's `repl` builds its header from `.code.file` and `.code.line`.

plan 6;

sub frame-code() { callframe(1).code }

my $code := frame-code();
is $code.^name, 'Block', 'the mainline frame\'s code is a Block';
nok $code ~~ Routine, '... not a Routine';
is $code.name, '<unit>', '... named <unit>';
is $code.file.IO.basename, $*PROGRAM.basename, '... in the program\'s file';
is $code.line, 1, '... starting at line 1';

sub outer() { frame-code() }
is outer().name, 'outer', 'a routine caller is still that routine';
