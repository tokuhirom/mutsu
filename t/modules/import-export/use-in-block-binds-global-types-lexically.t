use Test;
use lib 't/lib';

# A type a module exports from outside any package is a pad declaration of
# the block that `use`s the module, like a packaged one (ADR-8751, #14140).

plan 6;

sub outside { (try ::('BgeType').hi) // 'none' }

my &c = do { use BlockGlobalExportType; -> { BgeType.hi } };
is c(), 'bge', 'a closure taken in the block keeps the type';

{
    use BlockGlobalExportType;
    is BgeType.hi, 'bge', 'the block sees the type';
    my BgeType $x = BgeType.new;
    is $x.hi, 'bge', 'it is a type constraint in the block';
    is ::('BgeType').hi, 'bge', 'a by-name lookup in the block finds it';
    is outside(), 'none', 'a routine written outside the block does not see it';
}

is (try EVAL 'BgeType.hi') // 'gone', 'gone', 'the type is gone after the block';
