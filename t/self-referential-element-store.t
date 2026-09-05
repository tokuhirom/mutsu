use Test;

# `%h<k> = %h` / `@a[i] = @a` must store the container *itself*, so the
# structure is genuinely circular the way rakudo builds it. mutsu used to store
# an internal marker pair (`__mutsu_self_hash_ref` / `__mutsu_self_array_ref`)
# instead, which the cycle-aware renderers had never heard of: it leaked
# verbatim into `.gist` (`self => __mutsu_self_hash_ref => True`) and made
# `.raku` print a snapshot copy (`[1, [1]]`) rather than a back-reference.
#
# Addresses vary per run, so every rendering assertion normalises them away.
# The expected strings were taken from rakudo.

plan 18;

sub norm($s) { $s.subst(/ (Array|Hash|List|Map) '_' \d+ /, { "$0_ADDR" }, :g) }

# --- hash: the element store aliases, it does not copy -----------------------
my %h;
%h<a> = 1;
%h<self> = %h;

is norm(%h.gist), '(\Hash_ADDR = {a => 1, self => Hash_ADDR})',
   'gist of a self-assigned hash renders the back-reference';
is norm(%h.raku), '((my %Hash_ADDR) = {:a(1), :self(%Hash_ADDR)})',
   '.raku of a self-assigned hash renders the binding preamble';
ok !%h.gist.contains('__mutsu'), 'no internal marker leaks into .gist';
ok !%h.raku.contains('__mutsu'), 'no internal marker leaks into .raku';

ok %h<self> === %h, 'the stored value IS the hash, not a copy';
is %h<self><a>, 1, 'reading through the self-reference still works';
is %h.elems, 2, 'the self-reference is one ordinary entry';
is %h.keys.sort.join(','), 'a,self', 'keys are unaffected';

# A later write is visible through the self-reference -- the proof that the
# store aliased instead of snapshotting.
%h<b> = 2;
is %h<self><b>, 2, 'a later write is visible through the self-reference';
is %h<self><self><a>, 1, 'the reference is stable at any depth';

# A hash with nothing but the self-reference.
my %only;
%only<s> = %only;
is norm(%only.raku), '((my %Hash_ADDR) = {:s(%Hash_ADDR)})',
   'a hash holding only itself renders as a bare back-reference';

# --- array: same rule for an element store -----------------------------------
my @a;
@a[0] = 1;
@a[1] = @a;

is norm(@a.gist), '(\Array_ADDR = [1 Array_ADDR])',
   'gist of a self-assigned array element renders the back-reference';
is norm(@a.raku), '((my @Array_ADDR) = [1, @Array_ADDR])',
   '.raku of a self-assigned array element renders the binding preamble';
ok !@a.gist.contains('__mutsu'), 'no internal marker leaks into array .gist';

ok @a[1] === @a, 'the stored element IS the array, not a copy';
@a[0] = 9;
is @a[1][0], 9, 'a later write is visible through the self-reference';

# A single self-referential element keeps the real array's trailing comma.
my @only;
@only[0] = @only;
is norm(@only.raku), '((my @Array_ADDR) = [@Array_ADDR,])',
   'an array holding only itself keeps the trailing comma';

# --- copying a circular container snapshots the outer node -------------------
# `my %g = %h` builds a NEW hash whose `self` entry still points at the
# original, so the copy is acyclic and the preamble sits on the inner node.
my %src;
%src<a> = 1;
%src<self> = %src;
my %copy = %src;
is norm(%copy.gist), '{a => 1, self => (\Hash_ADDR = {a => 1, self => Hash_ADDR})}',
   'copying a circular hash keeps the cycle on the original node';

done-testing;
