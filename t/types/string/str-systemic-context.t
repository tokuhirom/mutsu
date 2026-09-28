use Test;

plan 12;

for $*KERNEL, $*DISTRO, $*VM -> $system {
    is ~$system, $system.Str, 'prefix ~ uses the system object Str method';
    is $system eq $system.Str, True, 'eq uses the system object Str method';
    is $system.Str eq $system.name, True, 'Str returns the system name';
    is "[$system]", "[{$system.Str}]", 'interpolation uses the system object Str method';
}
