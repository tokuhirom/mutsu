use Test;

# Two closures from one factory keep independent state, typed or not.
sub make-counter() { my Int $n = 0; sub { ++$n } }
my &a = make-counter;
my &b = make-counter;
a(); a();
is a(), 3, 'typed factory counter keeps its own state';
is b(), 1, 'sibling instance is independent';

sub make-acc() { my Str $s = ''; -> $x { $s ~= $x } }
my &acc = make-acc;
acc('a'); acc('b');
is acc('c'), 'abc', 'typed Str accumulator persists across calls';

# A nested whenever accumulates into a lexical of the enclosing whenever body,
# which has exited by the time its values arrive.
my $outer = Supplier.new;
my $inner = Supplier.new;
my @seen;
my $p = start react {
    whenever $outer.Supply {
        my Buf $buf = Buf.new;
        whenever $inner.Supply -> $v {
            $buf.push($v);
            @seen.push($buf.elems);
            done if $buf.elems == 3;
        }
    }
}
sleep 0.2;
$outer.emit(1);
sleep 0.2;
$inner.emit($_) for 1..3;
await Promise.anyof($p, Promise.in(5));
is-deeply @seen, [1, 2, 3], 'nested whenever accumulator persists across values';

# substr-rw window follows each STORE (shared length cell).
my $str = 'abcdef';
my $r := substr-rw($str, 1, 1);
$r = 'ZZ';
is $str, 'aZZcdef', 'substr-rw STORE splices';
$r = 'W';
is $str, 'aWcdef', 'substr-rw window tracks the stored length';

# A wrapper mutating its captured lexical is visible to the caller.
my $calls = 0;
sub wrapped($x) { $x * 2 }
&wrapped.wrap(-> |c { $calls++; callsame });
wrapped(1); wrapped(2);
is $calls, 2, 'wrapper counts through its captured lexical';

# cas block assigning an object to a captured lexical.
my $head = [1, 2, 3];
my $taken;
cas $head, -> $cur { $taken = $cur; [] };
is-deeply $taken, [1, 2, 3], 'cas block writes its captured object lexical';

# A closure called after its creator has rebound the captured variable.
my @subs;
for 1..3 -> $i {
    my $c = $i * 10;
    @subs.push: { $c++ };
}
is-deeply @subs.map({ $_() }).List, (10, 20, 30), 'loop closures first call';
is-deeply @subs.map({ $_() }).List, (11, 21, 31), 'loop closures keep per-iteration state';

done-testing;
