# Helper for t/modules/module-routine-try-reads-file-lexical.t: routines that
# read the module's file-scope lexicals inside `try`.
my $v = 5;
my Lock $lock .= new;
sub try-block is export { try { $v } }
sub try-statement is export { try $v * 2 }
sub try-lock is export { my $r; try { $lock.protect: { $r = $v + 1 } }; $r }
