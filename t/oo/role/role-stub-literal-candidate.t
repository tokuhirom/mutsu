use v6.d;
use Test;

# Red::Driver::Cache::Memory combines a role-required multi method with the
# base driver's literal empty-string overload.
plan 3;

role RequiredStringPrepare {
    multi method prepare(Str) { ... }
}

role LiteralEmptyPrepare {
    multi method prepare("") { 'empty' }
}

role GeneralStringPrepare {
    multi method prepare(Str) { 'general' }
}

role RequiredLiteralPrepare {
    multi method prepare("") { ... }
}

class MemoryLikeDriver
    does RequiredStringPrepare
    does LiteralEmptyPrepare
    does GeneralStringPrepare { }

my $driver = MemoryLikeDriver.new;
is $driver.prepare(''), 'empty',
    'the literal candidate wins for its exact value';
is $driver.prepare('other'), 'general',
    'the general candidate handles other strings';

dies-ok {
    EVAL q:to/END/;
        class MissingLiteralImplementation
            does RequiredLiteralPrepare
            does GeneralStringPrepare { }
        END
}, 'a general multi candidate does not satisfy a literal role stub';
