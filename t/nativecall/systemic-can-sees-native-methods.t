use Test;

plan 2;

ok $*DISTRO.^can('name').elems > 0,
    'Distro native methods are visible through ^can';
is $*DISTRO.name, 'linux',
    'the native Distro accessor remains callable';

done-testing;
