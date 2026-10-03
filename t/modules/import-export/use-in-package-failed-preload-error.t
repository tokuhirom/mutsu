use Test;

# A `use` inside a package block whose module body dies reports the module's
# own error, not a redeclaration of something the failed BEGIN-time preload
# had already registered (#11351).

plan 2;

use lib 't/lib';

my $err;
try {
    EVAL 'use PreloadDiesOuter;';
    CATCH { default { $err = .message } }
}
ok $err.defined, 'loading the module dies';
like $err, /'PreloadDiesDep body died'/, 'the module body error is reported';
