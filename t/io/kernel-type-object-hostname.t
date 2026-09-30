use Test;

# From Sys::Hostname: `Kernel.hostname` is callable on the type object.
plan 3;

isa-ok Kernel.hostname, Str, 'Kernel.hostname on the type object is a Str';
ok Kernel.hostname.chars > 0, 'Kernel.hostname is non-empty';
is Kernel.hostname, $*KERNEL.hostname, 'type object and $*KERNEL agree';
