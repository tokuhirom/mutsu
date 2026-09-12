use Test;

plan 7;

throws-like 'class AccessorCollision { has @.value; has &.value }',
    Exception,
    'public attributes with different sigils still cannot share an accessor';

class DuplicateAttributeSigils {
    has Int $!width = 42;
    has %!width;

    method scalar-width { $!width }
    method hash-width { %!width }
    method store-hash {
        %!width<answer> = 7;
        %!width<answer>
    }
}

my $object = DuplicateAttributeSigils.new;

is $object.scalar-width, 42, 'the scalar attribute keeps its value';
is $object.hash-width.^name, 'Hash', 'the hash attribute keeps its container kind';
is $object.store-hash, 7, 'the hash attribute has its own storage';
is $object.hash-width<answer>, 7, 'writes to the hash attribute do not hit the scalar';
is DuplicateAttributeSigils.^attributes.elems, 2, 'both sigil variants are introspectable';
is-deeply DuplicateAttributeSigils.^attributes.map(*.name).List,
    ('$!width', '%!width'),
    'attribute introspection distinguishes the sigils';
