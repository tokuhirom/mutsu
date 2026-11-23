use v6.e.PREVIEW;
use Test;
use lib 't/lib-client-map-block';
use ClientMapBlock;

# #18845: `CLIENT::` names the code that called in, a module's `.map` /
# `.grep` block included, though the block runs after its routine returned;
# and `$?PACKAGE` in such a block is the module's package.

plan 9;

my $rev;
my $pkg;
sub report { $rev = CLIENT::CORE::<CORE-SETTING-REV> }
sub report-both {
    $pkg := CLIENT::LEXICAL::<$?PACKAGE>;
    $rev = CLIENT::CORE::<CORE-SETTING-REV>;
}

callback-simple(&report);
is $rev, 'd', 'a one-statement routine called back from a 6.d module';

callback-in-map(&report);
is $rev, 'd', 'called back from a 6.d module\'s .map block';

callback-in-grep(&report);
is $rev, 'd', 'called back from a 6.d module\'s .grep block';

callback-in-map(&report-both);
is $rev, 'd', 'revision through .map with two statements';
is $pkg.^name, 'ClientMapBlock', 'the client package is the module\'s';

my module Inner {
    our $rev;
    our $pkg;
    our sub report {
        $pkg = CLIENT::LEXICAL::<$?PACKAGE>;
        $rev = CLIENT::CORE::<CORE-SETTING-REV>;
    }
}
callback-in-map(&Inner::report);
is $Inner::rev, 'd', 'a routine of another package called back through .map';
is $Inner::pkg.^name, 'ClientMapBlock', 'and its client package';

is-deeply package-in-map(), (ClientMapBlock,), '$?PACKAGE in a module routine\'s .map block';
is <x>.map({ $?PACKAGE.^name }).List, ('GLOBAL',), '$?PACKAGE in a mainline .map block';
