use Test;

# Docker::File's LABEL action passes a Seq of Pair values to a %.labels
# attribute.  Constructor coercion must materialize that sequence as a Hash.

plan 2;

class Labels {
    has Str %.labels;
}

my $pairs = ('name' => 'mutsu',).map({ $_ });
my $labels = Labels.new(labels => $pairs);

isa-ok $labels.labels, Hash, 'a Seq assigned to a hash attribute becomes a Hash';
is $labels.labels<name>, 'mutsu', 'the hash attribute keeps the sequence pairs';
