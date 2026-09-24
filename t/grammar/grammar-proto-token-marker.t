use Test;

plan 2;

grammar EmptyProto {
    proto token absent {*}
    token TOP { <absent> }
}

my $match = EmptyProto.subparse('', :rule<absent>);
ok !$match, 'an empty proto rule fails as a match';
$match = EmptyProto.subparse('', :rule<TOP>);
ok !$match, 'a caller can use an empty proto rule';
