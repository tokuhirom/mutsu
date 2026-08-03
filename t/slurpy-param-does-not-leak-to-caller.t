use lib 't/lib';
use Test;

# A callee's *slurpy* parameter must not survive into the caller's frame. The
# tree-walk return merge propagates a callee's `@`/`%` variables back to a
# same-named caller lexical (that is how a `%h` parameter's mutations reach the
# caller), but a slurpy is built fresh by the binder out of the leftover
# arguments, so propagating it just overwrites an unrelated caller binding.
#
# Reached through a module sub with a sigilless parameter, which is what keeps
# such a routine on the tree-walk fallback in the first place. The symptom in
# the wild: `Test.rakumod`'s `throws-like($code, $type, *%matcher)` came back
# from a nested `fails-like(..., *%matcher)` holding the *callee's* matcher, so
# its CATCH called `.instead` on an exception that only has `.message`
# (roast S24-testing/fails-like.t).

plan 3;

use SlurpyLeak;

is outer-named({ inner-named(sub {}, Int, :instead) }, message => 1),
    'message',
    "a callee's *%slurpy does not overwrite the caller's same-named slurpy";

is outer-positional({ inner-positional(sub {}, Int, 'callee') }, 'caller'),
    'caller',
    "a callee's *@slurpy does not overwrite the caller's same-named slurpy";

my %h = :start(1);
mutate-hash(sub {}, %h);
is %h<added>, 1, 'a non-slurpy %h parameter still writes its mutations back';
