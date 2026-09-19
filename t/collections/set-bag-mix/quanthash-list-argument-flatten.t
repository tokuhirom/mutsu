use v6.d;
use Test;

plan 2;

# Regression from ML::AssociationRuleLearning's Apriori singleton pruning.
my $items = <a b c>.map({ ($_ ,) }).List;
is Set($items).keys.sort.join(' '), 'a b c',
    'capitalized Set flattens a scalar List of one-item Lists';

my $nested = ($(<a b>),).SetHash;
is $nested.hash.keys[0][0], 'a',
    'method SetHash keeps an itemized List element as an object key';
