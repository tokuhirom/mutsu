use Test;

plan 7;

class X::WithPayload is X::AdHoc { }
my $ex = X::WithPayload.new(payload => 'message');
is $ex.payload, 'message', 'an X::AdHoc subclass inherits payload';
ok X::WithPayload.^can('payload'), 'the inherited accessor is introspectable';
is $ex.message, 'message', 'the inherited message uses the payload';
is $ex.gist, 'message', 'the inherited gist uses the payload';
throws-like { die $ex }, X::WithPayload, message => 'message',
    'throwing the subclass keeps its payload message';

class X::WithoutLineNumber is X::AdHoc {
    multi method gist(X::WithoutLineNumber:D:) { $.payload }
}
is X::WithoutLineNumber.new(payload => 'custom').gist, 'custom',
    'a custom gist can call the inherited payload accessor';

class X::Unrelated is Exception { }
dies-ok { X::Unrelated.new(payload => 'hidden').payload },
    'an unrelated Exception does not acquire a payload method';
