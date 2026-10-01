# Kernel.hostname works on the type object

`Kernel.hostname` (called on the `Kernel` type object rather than `$*KERNEL`) now returns the
host name, as Rakudo does. This is what `Sys::Hostname`'s `hostname` sub calls, so that
distribution's suite now passes.
