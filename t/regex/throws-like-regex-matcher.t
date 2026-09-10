use Test;

plan 7;

# `throws-like` smart-matches the thrown exception against its second argument
# (`$_ ~~ $ex_type` in Test.rakumod), so a Regex is a legal matcher: it is
# checked against the exception's stringification, not treated as a type name.

throws-like { die "some ad-hoc failure" }, /'ad-hoc failure'/,
    'a Regex matcher matches an X::AdHoc message';

throws-like { die "some ad-hoc failure" }, /^ 'some'/,
    'the Regex is anchored against the message, not a type name';

my $rx = /'in a variable'/;
throws-like { die "the pattern is in a variable" }, $rx,
    'a Regex held in a variable works as a matcher';

throws-like { die "MiXeD case" }, rx:i/'mixed CASE'/,
    'a Regex with adverbs honours them';

# A structured exception matches on its rendered message too.
throws-like { my Int $x = "str" }, /'Type check failed'/,
    'a Regex matcher matches a structured exception';

# The ordinary type-name form is unchanged.
throws-like { my Int $x = "str" }, X::TypeCheck::Assignment,
    'a type-object matcher still works';

throws-like { die "plain" }, X::AdHoc,
    'X::AdHoc still matches a die of a string';
