use Test;
use lib 't/lib';

plan 4;

use Issue8214ImportShadow <shadowed>;
sub shadowed() { "local" }
is shadowed(), "local", 'a local sub shadows a sub EXPORT import';
is &shadowed(), "local", 'the local declaration also owns the code symbol';

use Issue8214StaticExport;
sub static-shadow() { "local" }
is static-shadow(), "local", 'a local sub shadows a regular export';

throws-like {
    EVAL 'sub issue8214-duplicate() { 1 }; sub issue8214-duplicate() { 2 }'
}, X::Redeclaration, 'two local declarations still raise redeclaration';
