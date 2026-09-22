use v6.d;
use Test;

use lib 't/fixtures/compunit-meta6-provides';
use lib 't/fixtures/compunit-meta6-provides/lib';
use Mutsu::Meta6::Provided;

plan 1;

is provided-value(), 'loaded through META6 provides',
    'filesystem repositories resolve META6.json provides paths';
