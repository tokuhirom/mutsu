use Test;

# Pin for `CompiledCode::closure_body_arc`: every closure created from one
# `stmt_pool` slot now SHARES the block's AST (`SubData::body` is an
# `Arc<Vec<Stmt>>`) instead of deep-cloning it per creation. The bodies are
# immutable, so nothing observable may change — but two closures made from the
# same literal must still be independent objects with independent per-closure
# state.

plan 14;

# --- Two closures from the same literal are distinct objects -----------------
sub make-counter() { my $n = 0; return { $n++; $n } }
my $c1 = make-counter();
my $c2 = make-counter();
ok $c1 !=== $c2, 'two closures from the same literal are distinct objects';
is $c1(), 1, 'first closure counts from its own captured lexical';
is $c1(), 2, 'and keeps counting';
is $c2(), 1, 'the second closure has its own captured lexical';

# `state` inside a repeatedly-created block gets a fresh cell per closure.
sub make-stateful() { return { state $s = 0; $s++; $s } }
my $s1 = make-stateful();
my $s2 = make-stateful();
my @sseq; @sseq.push($s1()) for ^3;
is @sseq.join(','), '1,2,3', 'state persists within one closure';
is $s2(), 1, 'a separately-created closure gets its own state cell';

# --- A body with many statements still runs correctly ------------------------
my @results;
for 1, 2, 3 -> $i {
    my $f = -> $n {
        my $a = $n + $i;
        my $b = $a * 2;
        my $c = $b - 1;
        my $d = $c * $c;
        $d;
    };
    @results.push: $f(1);
}
is @results.join(','), '9,25,49', 'a multi-statement shared body computes per closure';

# --- Nested closures out of the same enclosing literal -----------------------
sub factory($base) { return -> $x { -> $y { $base + $x + $y } } }
my $f10 = factory(10);
my $f20 = factory(20);
is $f10(1)(2), 13, 'nested closure captures the outer factory argument';
is $f20(1)(2), 23, 'a second factory instance captures its own';
is $f10(5)(5), 20, 'the first one is unaffected by the second';

# --- Introspection over a shared body ----------------------------------------
my $blk = -> $x { $x + 1 };
is $blk.WHAT.gist, '(Block)', 'a pointy block still reports Block';
is $blk.arity, 1, 'signature introspection still works on a shared body';
is $blk(41), 42, 'and it still runs';

# --- A callback created inside a loop and stored ------------------------------
my @cbs;
for <a b c> -> $letter {
    @cbs.push: sub { $letter.uc }
}
is @cbs.map({ $_.() }).join(''), 'ABC',
    'stored callbacks from one literal keep separate captures';
