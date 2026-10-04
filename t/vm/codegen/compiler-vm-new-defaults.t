use Test;

# Distribution: META::constants (`Compiler.new.name`, `VM.new.name`).
# The Systemic classes default every attribute from the running process.

plan 5;

is Compiler.new.name, $*RAKU.compiler.name, "Compiler.new.name is the running compiler's";
is Compiler.new.version, $*RAKU.compiler.version, "Compiler.new.version too";
is VM.new.name, $*VM.name, "VM.new.name is the running VM's";
ok Distro.new.name.chars, "Distro.new.name is populated";
ok Kernel.new.name.chars, "Kernel.new.name is populated";
