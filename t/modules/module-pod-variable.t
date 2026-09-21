use v6.d;
use Test;
use lib 't/lib';
use EcosystemSwayConfigPod;

# Sway::Config uses Pod::Contents from its module body. Keep this regression
# fixture dependency-free while preserving the imported-module `$=pod` shape.
=NAME importing-program

plan 2;

is pod-name(), 'sway-config-fixture',
    'a loaded module sees its own Pod document';
is $=pod[0].contents[0].contents[0], 'importing-program',
    'loading a module restores the importing program\'s Pod document';
