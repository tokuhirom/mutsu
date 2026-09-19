use Test;

# ML::AssociationRuleLearning's ML::TriesWithFrequencies::Trie has this
# shape: an exported method and a same-named class-scoped helper sub.
plan 4;

class ExportedMethodWithSub {
    method leafQ(--> Str) is export { 'method' }

    sub leafQ(ExportedMethodWithSub $tr --> Str) {
        'sub'
    }

    method call-helper {
        leafQ(self)
    }
}

import ExportedMethodWithSub;
my $obj = ExportedMethodWithSub.new;

is leafQ($obj), 'method', 'the exported method remains the imported callable';
is $obj.call-helper, 'sub', 'the class-scoped helper sub remains callable from methods';
is &leafQ.WHAT.^name, 'Method', 'the imported callable keeps its Method identity';
is &leafQ.^name, 'Method', 'the imported callable reports Method as its name';
