use Test;

# `throws-like`'s named matchers used to be dropped entirely unless the thrown
# exception's class name started with `X::` and was not `X::AdHoc`. Since a bare
# `die "..."` and a `fail "..."` both produce an `X::AdHoc`, and a user-defined
# `class ... is Exception` is not named `X::...` either, every
#
#     throws-like { die "x" }, X::AdHoc, message => /"ZZZ"/
#
# passed VACUOUSLY: the matcher never ran and the subtest plan undercounted
# (1..2 instead of rakudo's 1..3). The matchers now always run -- rakudo's
# `throws-like` calls `$x."$k"()` for every named matcher and always plans
# `2 + %matcher.elems` tests.

plan 14;

class MyErr is Exception {
    has $.code;
    method message() { "my error $!code" }
}

# --- the matcher actually runs, for every matcher shape --------------------

throws-like { die "hello world" }, X::AdHoc, message => /world/;
throws-like { die "hello world" }, X::AdHoc, message => 'hello world';
throws-like { die "hello world" }, X::AdHoc, message => *.contains('hello');
throws-like { die "hello world" }, X::AdHoc, gist => /world/;

# `payload` is a genuine X::AdHoc attribute -- `die "x"` stores its argument
# there -- so a `payload =>` matcher must be answered too.
throws-like { die "hello" }, X::AdHoc, payload => 'hello';

# `fail "..."` produces an X::AdHoc just like `die` does.
sub frodo(Bool :$destroys-ring) {
    fail "Oops. Frodo dies" unless $destroys-ring
}
throws-like { frodo }, Exception, message => /dies/;

# A Str first argument (the code is EVAL'd) takes matchers the same way.
throws-like 'die "boom"', X::AdHoc, message => /boom/;

# --- a user-defined exception class is not `X::...`, but still matchable ---

throws-like { die MyErr.new(code => 5) }, MyErr, code => 5;
throws-like { die MyErr.new(code => 5) }, MyErr, message => /'my error 5'/;

# --- a per-type attribute on a real X:: subclass keeps working -------------

throws-like { Int('zz') }, X::Str::Numeric, source => /zz/;

# --- the negative case: prove the matcher really can FAIL now --------------
#
# This must be checked in a child process: a failing `throws-like` inside this
# file would fail the file. Asserting the child's *plan line* as well as the
# `not ok` is the point -- a test that only checked for a pass would itself be
# vacuous, which is exactly the bug being fixed here.

my $script = '_throws_like_message_matcher_child.raku';
spurt $script, q:to/END/;
use Test;
plan 2;
throws-like { die "actual message" }, X::AdHoc, message => /"NOPE"/;
throws-like { die "actual message" }, X::AdHoc, message => /actual/;
END

my $proc = run($*EXECUTABLE, $script, :out, :!err);
my $out := $proc.out.slurp;
my $exit = $proc.exitcode;

like $out, / ^^ \s* '1..3' /,
    'the subtest plans 2 + one matcher (1..3), not 1..2';
like $out, / ^^ \s* 'not ok 3 - .message matches' /,
    'a message matcher that does not match actually FAILS';
like $out, / ^^ \s* 'ok 3 - .message matches' /,
    'a message matcher that does match still passes';
isnt $exit, 0, 'the child run reports failure';

unlink $script;
