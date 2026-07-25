unit class ResInner;

# Read at BEGIN time, from the module's own mainline — the shape
# OpenSSL::NativeLib uses. `%?RESOURCES` here must resolve against THIS
# distribution, not against whichever distribution the routine that triggered
# the load happens to belong to.
my $greeting = BEGIN %?RESOURCES<greeting.txt>.slurp(:close).trim;

method greeting() { $greeting }
