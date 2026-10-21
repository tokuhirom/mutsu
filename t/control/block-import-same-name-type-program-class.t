use Test;
use lib 't/lib';

# A block-scoped `use` that imports a type under a name the program declares
# itself (`my class`) shadows it for the block only. A top-level routine the
# block calls still sees the program's own class (ADR-8751).
plan 6;

my class SameNameCls { method hi { "program" } }
sub callee { SameNameCls.new.hi }
sub callee-name { SameNameCls.^name }
my $keep;
{
    use BlockImportSameNameType;
    is SameNameCls.new.hi, 'module', 'inside the block the import wins';
    is callee(), 'program', 'a top-level routine called from the block sees the program class';
    is callee-name(), 'SameNameCls', 'also through a meta-method call';
    $keep = -> { SameNameCls.new.hi };
}
is SameNameCls.new.hi, 'program', 'after the block the program class is back';
is callee(), 'program', 'and the routine still sees it';
is $keep(), 'module', 'a closure taken out of the block keeps the import';
