use Test;

# From Sys::Hostname: `Kernel.hostname` is callable on the type object.
plan 8;

isa-ok Kernel.hostname, Str, 'Kernel.hostname on the type object is a Str';
ok Kernel.hostname.chars > 0, 'Kernel.hostname is non-empty';
is Kernel.hostname, $*KERNEL.hostname, 'type object and $*KERNEL agree';

# The other `Systemic` facts on a type object (#13952): `Raku.version` is the
# language revision; `Kernel`'s attributes need an instance.
is Raku.version, $*RAKU.version, 'Raku.version on the type object';
throws-like { Kernel.name }, Exception,
    message => /'Cannot look up attributes in a Kernel type object'/, 'Kernel.name';
throws-like { Kernel.release }, Exception,
    message => /'Cannot look up attributes in a Kernel type object'/, 'Kernel.release';
throws-like { Kernel.signals }, X::Parameter::InvalidConcreteness, 'Kernel.signals';
is $*KERNEL.name, $*KERNEL.name.Str, 'an instance still reads its name';
