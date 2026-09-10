use Test;

plan 8;

# Raku's `LHS xx N` re-evaluates a side-effecting / non-deterministic LHS on each
# of the N repetitions. mutsu re-runs a known set of such calls; plain values are
# replicated.

# Mutating method runs on each repetition (regression: HTTP::HPACK builds a
# Huffman tree with `@tree.push(0) xx 2`).
my @push;
@push.push(0) xx 3;
is @push.elems, 3, '.push(0) xx 3 pushes three times';

my @app;
@app.append(1, 2) xx 2;
is @app.elems, 4, '.append xx 2 appends twice';

my @un;
@un.unshift(9) xx 2;
is @un.elems, 2, '.unshift xx 2 runs twice';

# rand is re-evaluated, so the values vary (not all identical).
my @r = (^1000).pick xx 20;
ok @r.unique.elems > 1, '.pick xx N re-evaluated (values vary)';

# Plain literal is replicated.
is-deeply (42 xx 3).List, (42, 42, 42).List, 'literal replicated';

# Variable read is replicated.
my $v = 7;
is-deeply ($v xx 3).List, (7, 7, 7).List, 'variable read replicated';

# A block literal is replicated as a value (NOT executed).
my $side = 0;
my @blocks = { $side++ } xx 3;
is $side, 0, 'block literal not executed by xx';

# Zero count produces an empty list.
my @z;
is (@z.push(1) xx 0).elems, 0, 'xx 0 produces empty list';
