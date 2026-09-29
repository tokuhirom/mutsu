use Test;

plan 2;

# X::Proc::Unsuccessful names the program (`$.proc.command[0]`), not the whole command line.
try { run "sh", "-c", "exit 3"; 1 }
is $!.message, "The spawned command 'sh' exited unsuccessfully (exit code: 3, signal: 0)",
    'message names only the program';
isa-ok $!, X::Proc::Unsuccessful, 'sinking a failed run throws X::Proc::Unsuccessful';
