use Test;

plan 4;

my $match = "aéfooZ".match(/foo/);
is $match.replace-with("X"), "aéXZ", 'replace-with preserves the subject around the match';
is $match.replace-with(42), "aé42Z", 'replacement is stringified';
is "abc".match(/z/).replace-with("X"), Nil, 'failed match returns Nil';
is "foo foo".match(/foo/, :g)[0].replace-with("bar"), "bar foo",
    'replacement uses the selected match span';
