use lib $?FILE.IO.parent.add('lib').Str;
use Test;
use ScalarHeldLexical;

# A module's file-scope SCALAR-held container (`my $a = [...]`) must be private
# to the module, exactly like the `@`/`%` shapes
# `t/container-lexical-declarator-matrix.t` pins. Two distinct bugs used to make
# the module's routines mutate -- or permanently rebind -- the CONSUMER's
# same-named `my $a` instead:
#
#   1. The sigil-less array mutator arms (`push`/`append`/`unshift`/`prepend`/
#      `pop`/`shift`) in `runtime/methods_mut_dispatch.rs` read and wrote the
#      raw `self.env` entry under the bare name, skipping the
#      `env_root_descended_mut` chokepoint the `@`/`%` arms use. `our $a` had no
#      chokepoint arm at all.
#   2. A method of a class declared IN the module carries the module mainline as
#      its `captured_env`. An authoritative capture overwrites the caller's
#      same-named env entry on entry, and `merge_method_env` then merged that
#      value back into the caller -- so a method that merely READ the module's
#      container repointed the consumer's own variable at it.
#
# Every assertion here is byte-identical under `raku`.
#
# The mainline `sub` below is load-bearing, not decoration: a consumer with no
# mainline named sub never mirrors its `my $arr` into `env`, so bug 1 stayed
# invisible in a two-line reduction of the same program.

plan 24;

sub trigger() { 1 }

my $arr = [<x y z>];
my $hsh = { mine => 1 };
my $ours = [<x y z>];
my $st = [<x y z>];
my @carr = <x y z>;
my $cscalar = "consumer";

sh-push("P");
is sh-peek(), 'a,b,c,P', 'module my $-held array: push hits the module binding';
is $arr.join(","), 'x,y,z', "consumer's same-named my \$ array is untouched (push)";

sh-append("A");
is sh-peek(), 'a,b,c,P,A', 'module my $-held array: append hits the module binding';
is $arr.join(","), 'x,y,z', "consumer's my \$ array is untouched (append)";

sh-unshift("U");
is sh-peek(), 'U,a,b,c,P,A', 'module my $-held array: unshift hits the module binding';
is $arr.join(","), 'x,y,z', "consumer's my \$ array is untouched (unshift)";

sh-prepend("R");
is sh-peek(), 'R,U,a,b,c,P,A', 'module my $-held array: prepend hits the module binding';
is $arr.join(","), 'x,y,z', "consumer's my \$ array is untouched (prepend)";

is sh-pop(), 'A', 'module my $-held array: pop removes from the module binding';
is sh-shift(), 'R', 'module my $-held array: shift removes from the module binding';
is $arr.join(","), 'x,y,z', "consumer's my \$ array is untouched (pop/shift)";

is sh-splice().join(","), 'U', 'module my $-held array: splice cuts the module binding';
sh-elemset('Z');
is sh-peek(), 'Z,b,c,P', 'module my $-held array: element assign hits the module binding';
is $arr.join(","), 'x,y,z', "consumer's my \$ array is untouched (splice/element)";

sh-hset("m");
sh-hpush("n");
is sh-hpeek(), 'k,m,n', 'module my $-held hash sees its own binding';
is $hsh.keys.sort.join(","), 'mine', "consumer's same-named my \$ hash is untouched";

sh-our-push("O");
is sh-our-peek(), 'a,b,c,O', 'module our $-held array sees the package binding';
is $ours.join(","), 'x,y,z', "consumer's same-named my \$ array is untouched (our)";

sh-state-push("S");
is sh-state-peek(), 'a,b,c,S', 'module state $-held array sees its own binding';
is $st.join(","), 'x,y,z', "consumer's same-named my \$ array is untouched (state)";

# A method of a module-declared class must not rebind the consumer's lexicals.
my $holder = ScalarHeldHolder.new;
$holder.mpush("M");
is $holder.mpeek(), 'Z,b,c,P,M', 'a module class method mutates the module binding';
is $arr.join(","), 'x,y,z', "a module class method leaves the consumer's \$ alone";

$holder.apeek();
$holder.speek();
is @carr.join(","), 'x,y,z',
    "a module class method's captured env does not rebind the consumer's \@";
is $cscalar, 'consumer',
    "a module class method's captured env does not rebind the consumer's \$";
