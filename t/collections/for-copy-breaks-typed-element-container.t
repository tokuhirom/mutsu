use Test;

plan 2;

class LoopCopyContainerSource { }

my LoopCopyContainerSource $source .= new;
my @items;
@items[0] := $source;

lives-ok {
    for @items -> $item is copy {
        $item = 'independent copy';
    }
}, 'is copy can replace a typed element with a different type';

isa-ok $source, LoopCopyContainerSource,
    'the original typed element remains unchanged';
