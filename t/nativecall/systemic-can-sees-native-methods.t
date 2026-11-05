use Test;

plan 2;

ok $*DISTRO.^can('name').elems > 0,
    'Distro native methods are visible through ^can';
# The name is the host's (os-release ID on Linux), so compare with the object's
# own stringification rather than a hardcoded distribution.
is $*DISTRO.name, $*DISTRO.Str,
    'the native Distro accessor remains callable';

done-testing;
