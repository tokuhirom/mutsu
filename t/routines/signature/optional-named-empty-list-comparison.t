use v6;
use Test;

plan 2;

sub accepts-http(:$http) {
    my $http-val = $http // ();
    so $http-val == <1.1>|()
}

ok accepts-http(), 'an omitted optional named list compares equal to an empty list';
ok accepts-http(http => <1.1>), 'the same comparison accepts the HTTP/1.1 value';
