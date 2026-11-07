use Test;
use lib 't/lib';

plan 5;

{
    use ExportOutside::Rep::CI;
    is EmptyCI.new.id, 'empty', 'a class exported from A/B/CI.rakumod as A::B::EmptyCI is imported by its last part';
    ok EmptyCI.new ~~ CI, 'and does the role the module is named after';
}

# The importer itself never mentions ExportOutside::Thing: the attribute type
# `Thing` of Holder is Holder's own import, not something the running package
# (here, the main program) has to know about.
use ExportOutside::Holder;
my $h = Holder.new;
is $h.hi, 'hi', 'a typed attribute naming an imported class accepts its default';
ok $h.thing.defined, 'and holds an object';
is $h.thing.^name, 'ExportOutside::Thing', 'of the imported class';
