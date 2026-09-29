# X::Proc::Unsuccessful names the program, not the command line

Sinking a failed `run "sh", "-c", "exit 3"` now reports `The spawned command 'sh' exited
unsuccessfully (...)`, matching Rakudo, which uses `$.proc.command[0]`. Previously the whole
command line was stringified into the message.
