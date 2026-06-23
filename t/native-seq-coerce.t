use Test;

# `.Seq` coercion native dispatch (VM-native, shared impl with the interpreter
# via `builtins::seq_coerce::to_seq_structural`). Structural receivers
# (Seq/Array/Slip/Range/bare scalar) go native; Supply/LazyList/Instance keep
# the interpreter's state-touching handling. Behavior must match raku.

plan 16;

# --- structural receivers (native) ----------------------------------------
is (1, 2, 3).Seq.^name, 'Seq', 'List.Seq is a Seq';
isa-ok [1, 2, 3].Seq, Seq, 'Array.Seq is a Seq';
is [1, 2, 3].Seq.elems, 3, 'Array.Seq elems';
is (1..5).Seq.elems, 5, 'Range.Seq elems';
is (1..^5).Seq.elems, 4, 'exclusive Range.Seq elems';
is <a b c>.Seq.join(','), 'a,b,c', 'word-list.Seq contents';
is (1, 2, 3).Seq.Seq.elems, 3, 'Seq.Seq is idempotent';

# --- bare scalar wraps into a one-element Seq -----------------------------
is 'x'.Seq.elems, 1, 'Str.Seq is one element';
is 42.Seq.elems, 1, 'Int.Seq is one element';
is 42.Seq.[0], 42, 'Int.Seq element value';

# --- Slip flattens (interpreter and native agree) -------------------------
is (slip(1, 2), 3).Seq.elems, 3, 'Slip in list.Seq flattens';

# --- variable receiver (mut path) -----------------------------------------
my @a = 10, 20, 30;
is @a.Seq.^name, 'Seq', '@a.Seq is a Seq (variable receiver)';
is @a.Seq.list.join(','), '10,20,30', '@a.Seq contents';
is @a.elems, 3, 'receiver array unchanged';

# --- gather/lazy still works via the interpreter carrier ------------------
my $g = (gather { take 1; take 2; take 3 }).Seq;
is $g.elems, 3, 'gather.Seq drains via interpreter (carrier)';

# --- empty ----------------------------------------------------------------
is ().Seq.elems, 0, 'empty list .Seq is empty';
