use Test;

# The once-evaluated condition of `with`/`without`/`orwith` lives in a local
# slot, stored raw (#18208). Everything the by-name temp it replaced did must
# still hold: the topic aliases an lvalue source, a List is not itemized, a
# Proxy reaches its STORE, an rw call's element is written through, the
# condition runs once, and nested and looped `with`s keep their own temps.

plan 15;

{
    my $x = 1;
    with $x { $_ = 5 }
    is $x, 5, 'with $x writes back through the topic';
}
{
    my %h = k => 1;
    with %h<k> { $_ = 7 }
    is-deeply %h, {k => 7}, 'with %h<k> writes back to the element';
}
{
    my @a = 1, 2;
    with @a[1] { $_ = 9 }
    is-deeply @a, [1, 9], 'with @a[i] writes back to the element';
}
{
    my %h;
    sub slot($k) is rw { %h{$k}{1} }
    without slot(4) { $_ = 5 }
    is-deeply %h, {4 => {1 => 5}}, 'without over an rw call binds the topic to the element';
}
{
    my $elems;
    with (1, 2, 3) -> @m { $elems = @m.elems }
    is $elems, 3, 'a List condition is not itemized';
}
{
    my @stored;
    sub lv() is rw { my $v = 3; Proxy.new(FETCH => { $v }, STORE => -> $, $n { @stored.push($n) }) }
    with lv() { $_ = 'a' }
    is-deeply @stored, ['a'], 'a Proxy condition reaches its STORE';
}
{
    my $calls = 0;
    sub once-c { $calls++; 42 }
    my $seen;
    with once-c() { $seen = $_ }
    is "$seen $calls", '42 1', 'the condition runs once';
}
{
    my $r = '';
    with Nil { $r = 'with' } orwith 0 { $r = "orwith $_" } else { $r = 'else' }
    is $r, 'orwith 0', 'orwith takes its own temp';
    with Nil { $r = 'with' } orwith Nil { $r = 'orwith' } else { $r = 'else' }
    is $r, 'else', 'else when neither is defined';
}
{
    my $r = 'unset';
    without 5 { $r = 'no' }
    is $r, 'unset', 'without skips a defined value';
}
{
    my $f = Failure.new('x');
    my $r;
    with $f { $r = 'defined' } else { $r = 'undefined' }
    is $r, 'undefined', 'a Failure is undefined';
}
{
    my @out;
    for 1 .. 4 {
        with $_ %% 2 ?? Nil !! $_ * 10 { @out.push($_) }
    }
    is-deeply @out, [10, 30], 'a with in a loop gets a fresh value each iteration';
}
{
    my @out;
    with 1 {
        my $outer = $_;
        with $outer + 1 { @out.push("$outer $_") }
        @out.push("after $_");
    }
    is-deeply @out, ['1 2', 'after 1'], 'nested withs keep their own temps and topics';
}
{
    sub w($s) { my $x = 0; with $s.index('/', 1) { $x = $_ }; $x }
    is w('/page/42'), 5, 'with over a method call in a sub';
    is w('page'), 0, 'and its undefined branch';
}
