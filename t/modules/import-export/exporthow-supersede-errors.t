use lib 't/lib';
use Test;

# EXPORTHOW imports that cannot work are compile-time errors (#14536).

plan 6;

throws-like { EVAL 'use SupersedeNothing;' },
    X::EXPORTHOW::NothingToSupersede, declarator => 'no-such-declarator',
    'superseding a declarator that does not exist';

throws-like { EVAL 'use SupersedeClassA; use SupersedeClassB;' },
    X::EXPORTHOW::Conflict, directive => 'SUPERSEDE', declarator => 'class',
    'two modules superseding the same declarator';

throws-like { EVAL 'use DeclareBuiltinClass;' },
    X::EXPORTHOW::Conflict, directive => 'DECLARE', declarator => 'class',
    'declaring a built-in declarator again';

lives-ok { EVAL 'use SupersedeClassA; class Y { }; Y.new' }, 'a single supersede works';

try { EVAL 'use SupersedeNothing;' };
like $!.message, /'no package declarator \'no-such-declarator\' to supersede'/, 'message';
try { EVAL 'use SupersedeClassA; use SupersedeClassB;' };
like $!.message, /"'EXPORTHOW::SUPERSEDE::class' conflicts with an existing meta-object"/, 'conflict message';
