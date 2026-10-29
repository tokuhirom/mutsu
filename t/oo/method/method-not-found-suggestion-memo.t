use Test;

# The "Did you mean" suggestions of X::Method::NotFound are memoized per
# (type, method); a repeated miss must word the message exactly as the first.
plan 10;

sub miss(&call) {
    my $message = '';
    try { call(); CATCH { default { $message = .message } } }
    $message
}

my $first = miss({ 5.elemz });
my $again = miss({ 5.elemz });
like $first, /'Did you mean' \s+ "'elems'?"/, 'a near miss suggests the real method';
is $again, $first, 'the repeated miss words the same message';

my $other-type = miss({ "abc".elemz });
like $other-type, /"for string 'abc'"/, 'another invocant keeps its own wording';
like $other-type, /'Did you mean' \s+ "'elems'?"/, 'and still gets the suggestion';

my $none = miss({ 5.xyzzyplugh });
unlike $none, /'Did you mean'/, 'a far miss suggests nothing';
is miss({ 5.xyzzyplugh }), $none, 'also when repeated';

my \T = Int;
my $hits = 0;
for ^200 { $hits++ without T.?inflator }
is $hits, 200, '.?name on a type without the method answers Nil every time';
ok T.?gist.defined, '.?name still calls a method that exists';

class Foo { method hit { 42 } }
is Foo.?hit, 42, '.?name calls a user method';
nok Foo.?miss.defined, '.?name on a missing user method is undefined';
