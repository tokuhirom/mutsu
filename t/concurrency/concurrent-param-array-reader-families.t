use Test;

# #13016's acceptance harness. Eight threads share one parameter array that
# reaches them through a nested routine's closure (a `:=`-style cell once a
# `start` captures it). Half of them restructure it (`push`/`shift`, `splice`,
# element `:delete`); the other half read it, each family both directly and
# through a routine call whose `@x` parameter writes back into the caller's
# cell on return. Unsynchronized concurrent mutation has no defined answer in
# Raku, so these pin MUTSU's guarantee, not a Raku one: every shape finishes,
# never a panic or a corrupted heap. Before the cell's stores and clone-reads
# took its ADR-0068 stripe, the call-site writeback raced the mutators and the
# main thread died on `Gc::drop strong-count underflow`.

plan 9;

sub through-call(@x, &read) { read(@x) }

sub drive(&read) {
    sub go(@x) {
        await (^8).map: -> $i {
            start {
                for ^300 -> $k {
                    if $i %% 2 {
                        read(@x);
                        through-call(@x, &read);
                    }
                    elsif $i == 1 { @x.push($k); @x.shift }
                    elsif $i == 3 { @x.splice(1, 1, $k) }
                    elsif $i == 5 { @x[2]:delete; @x[2] = $k }
                    else          { @x.unshift($k); @x.pop }
                }
            }
        }
    }
    my @b = ^20;
    go(@b);
    @b.elems
}

ok drive(-> @x { so @x }),          'boolean test';
ok drive(-> @x { +@x }),            'numeric coercion';
ok drive(-> @x { ~@x }),            'stringification';
ok drive(-> @x { my $n = 0; $n++ for @x; $n }), 'iteration';
ok drive(-> @x { @x[0] }),          'element read';
ok drive(-> @x { @x.elems }),       '.elems';
ok drive(-> @x { @x ~~ Positional }), 'smartmatch';

# The writeback itself: a routine that returns without touching its `@x`
# must not put back an older copy over another thread's pushes.
{
    my @b = ^20;
    sub rd(@x) { @x.elems }
    sub go(@x) {
        await (^8).map: -> $i {
            start { for ^250 -> $k { if $i %% 2 { rd(@x) } else { @x.push($k) } } }
        }
    }
    go(@b);
    is @b.elems, 20 + 4 * 250, 'a non-mutating callee loses no concurrent push';
}

# Sequential use of the same shape still answers exactly.
{
    my @solo = ^5;
    sub bump(@x) { @x.push(5); @x.splice(0, 1) }
    bump(@solo);
    is-deeply @solo, [1, 2, 3, 4, 5], 'single-threaded, the same calls answer exactly';
}
