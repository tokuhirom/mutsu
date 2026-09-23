use Test;

use lib 't/lib';
use CustomExportBuiltinShadow <any>;

plan 1;
is any({ True }, 1, 2), 'custom-any',
    'an imported custom EXPORT code alias shadows a builtin';
