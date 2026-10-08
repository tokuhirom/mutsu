unit class FinishDataModule;

# The module's own `=finish` section, read at load time and from a routine.
my @rows = $=finish.lines.grep(*.chars);

method rows() { @rows }
method late() { $=finish.lines.elems }

=finish
alpha:1
beta:2
gamma:3
