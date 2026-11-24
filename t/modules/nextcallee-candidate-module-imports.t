use v6;
use Test;
use lib $?FILE.IO.parent(2).add('lib').Str;

plan 3;

# A method candidate handed out by `nextcallee` runs in its own compunit: it
# calls a routine its declaring module imports even when the `nextcallee` call
# site sits in another module that never imported it (#19172; Red's
# `Red::Transaction::Guard` role reaching `Red::Driver.prepare`, which calls the
# imported `protected-row-query`).
use NextcalleeImports::Driver;
use NextcalleeImports::Scoped;

my $driver = NextcalleeImports::Driver.new but Scoped;
is $driver.prepare(42), 'helper(42)',
    'a role mixed in from another module defers to a candidate that calls its own import';

role LocalScoped {
    method prepare(|c) {
        my &next = nextcallee;
        'local>' ~ next(self, |c)
    }
}
is (NextcalleeImports::Driver.new but LocalScoped).prepare(7), 'local>helper(7)',
    'a role declared in the program defers the same way';

nok MY::<&nci-helper>:exists, 'the program itself never imported the helper';
