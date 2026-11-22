use v6.e.PREVIEW;
use lib $?FILE.IO.parent(3).add('lib');
use Test;

# CLIENT:: names the code that called into the running routine's package, and
# a bare word behind a chain of pseudo-packages misses quietly (#18742).

plan 8;

use ClientRevision6d;

is my-client-rev(), 'e', 'a 6.d routine\'s client is this 6.e file';
is my-client-pkg().^name, 'GLOBAL', '... in GLOBAL';

my $rev;
my $pkg;
sub report {
    $pkg := CLIENT::LEXICAL::<$?PACKAGE>;
    $rev = CLIENT::CORE::<CORE-SETTING-REV>;
}
call-back(&report);
is $rev, 'd', 'called back from the 6.d module, the client is 6.d code';
is $pkg.^name, 'ClientRevision6d', '... of the module\'s package';

eval-lives-ok 'CORE::UNIT::True', 'CORE::UNIT::True lives';
eval-lives-ok 'CORE::CALLERS::CORE::True', 'CORE::CALLERS::CORE::True lives';
eval-lives-ok 'OUTER::' x 50 ~ 'True', 'OUTER:: past the outermost scope lives';
eval-lives-ok 'CALLER::' x 50 ~ 'True', 'CALLER:: past the outermost caller lives';
