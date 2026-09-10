use Test;

# `.gist` of a circular container used to abort the whole process with a stack
# overflow -- not an exception a CATCH could see, but a hard `fatal runtime
# error`. Five separate walks recursed without a cycle guard: the two
# dispatch probes that decide whether the native gist fast path applies
# (`vm_native_dispatch`, `dispatch_core_repr`), the `say` fast-path probe
# (`vm_data_io_ops`), the interpreter-side probe (`methods_call_dispatch`), and
# the native per-type renderer itself.
#
# Rakudo renders a cycle the way `Mu.gistseen` does: the node the walk loops
# back to is named `<Type>_<address>`, and *that* node carries a `(\Name = ...)`
# binding preamble. Addresses vary per run, so every assertion here normalises
# them away.

plan 11;

sub norm($s) { $s.subst(/ (Array|Hash|List|Map) '_' \d+ /, { "$0_ADDR" }, :g) }

# --- the headline repro: a self-referential array ---------------------------
my @c;
@c = 42, @c;
is norm(@c.gist), '(\Array_ADDR = [42 Array_ADDR])',
   'gist of a self-referential array renders the back-reference';
# Repeated calls must agree: the visited set is popped on the way out, so no
# state leaks from one render into the next.
is @c.gist, @c.gist, 'gist is stable across calls (the visited set does not leak)';

# The name in the preamble and the name at the back-reference must be the same
# node, i.e. the real address is used, not a counter.
my $raw = @c.gist;
my $name = $raw ~~ / '(\\' (\w+) ' = ' / ?? ~$0 !! '';
ok $name.chars > 6 && $raw.contains("[42 $name]"),
   'preamble name and the back-reference name are the same node';

# --- a cycle that closes one level down -------------------------------------
my @d;
@d = 1, [2, @d];
is norm(@d.gist), '(\Array_ADDR = [1 [2 Array_ADDR]])',
   'a cycle through a nested array still names the outermost looping node';

# --- an array that contains only itself -------------------------------------
my @e;
@e.push(@e);
is norm(@e.gist), '(\Array_ADDR = [Array_ADDR])',
   'an array holding only itself gists as a bare back-reference';

# --- the preamble attaches to the looping node, NOT the top level -----------
# Rakudo puts `(\... = ...)` on the node the walk actually loops back to, so an
# acyclic outer array keeps its plain brackets.
my @outer;
my @inner;
@inner = 1, @inner;
@outer = 0, @inner;
is norm(@outer.gist), '[0 (\Array_ADDR = [1 Array_ADDR])]',
   'the binding preamble sits on the cycle root, not on the top-level node';

# --- a cycle reached through a hash -----------------------------------------
my @m;
my %n;
@m = 1, %n;
%n<x> = @m;
is norm(@m.gist), '(\Array_ADDR = [1 {x => Array_ADDR}])',
   'a cycle running through a hash renders the back-reference';

# --- a `:=`-bound element closing the cycle ---------------------------------
my @b;
@b[0] = 1;
@b[1] := @b;
is norm(@b.gist), '(\Array_ADDR = [1 Array_ADDR])',
   'a bind-closed cycle renders the back-reference';

# --- a DAG is NOT a cycle ---------------------------------------------------
# The same container reachable by two *non-nested* paths is rendered in full at
# each occurrence: the visited set is ancestor-scoped, not walk-global.
my @shared = 1, 2;
my @dag;
@dag[0] := @shared;
@dag[1] := @shared;
is norm(@dag.gist), '[[1 2] [1 2]]',
   'a shared-but-acyclic array is rendered in full at both occurrences';

my %shared = a => 1;
my @dagh;
@dagh[0] := %shared;
@dagh[1] := %shared;
is norm(@dagh.gist), '[{a => 1} {a => 1}]',
   'a shared-but-acyclic hash is rendered in full at both occurrences';

# --- ordinary containers are untouched --------------------------------------
is [1, [2, 3], {b => 4}].gist, '[1 [2 3] {b => 4}]',
   'a plain nested container still gists normally';

done-testing;
