use Test;

plan 12;

# Track B T6: a closure whose ONLY use of an outer aggregate is an element
# increment/decrement or `:delete` must still capture it — the mutation has to
# survive the closure escaping its declaring frame (verified against raku).

# hash element ++ through an escaped closure pair
sub hash-bump() {
    my %h;
    return sub ($k) { %h{$k}++ }, sub () { %h.elems };
}
my ($bump, $count) = hash-bump();
$bump('x');
$bump('y');
is $count(), 2, 'escaped %h{$k}++ lands in the shared hash';
$bump('x');
is $count(), 2, 'repeat key increments in place';

# array element ++ / -- through an escaped closure pair
sub arr-bump() {
    my @a = 10, 20;
    return sub ($i) { @a[$i]++ }, sub ($i) { @a[$i]-- }, sub () { @a.join(',') };
}
my ($inc, $dec, $show) = arr-bump();
$inc(0); $inc(0); $inc(1);
is $show(), '12,21', 'escaped @a[$i]++ lands in the shared array';
$dec(1);
is $show(), '12,20', 'escaped @a[$i]-- lands too';

# hash :delete through an escaped closure pair
sub hash-del() {
    my %h = a => 1, b => 2;
    return sub ($k) { %h{$k}:delete }, sub () { %h.keys.sort.join(',') };
}
my ($del, $keys) = hash-del();
$del('a');
is $keys(), 'b', 'escaped %h{$k}:delete removes the key for every sharer';

# array :delete through an escaped closure pair
sub arr-del() {
    my @a = 1, 2, 3;
    return sub ($i) { @a[$i]:delete }, sub () { @a.elems };
}
my ($adel, $alen) = arr-del();
$adel(2);
is $alen(), 2, 'escaped @a[$i]:delete shortens the array';

# pre-increment form
sub pre-bump() {
    my %h;
    return sub ($k) { ++%h{$k} }, sub () { %h.elems };
}
my ($pbump, $pcount) = pre-bump();
$pbump('k1');
is $pcount(), 1, 'escaped ++%h{$k} lands too';

# mutation via one closure visible through another registered in an outer array
my @subs;
sub reg() {
    my %seen;
    @subs.push(sub ($k) { %seen{$k}++ });
    @subs.push(sub () { %seen.elems });
}
reg();
@subs[0]('x');
@subs[0]('y');
is @subs[1](), 2, 'closures registered via outer array share the captured hash';

# pre-escape mutations and post-escape mutations accumulate in the same store
sub mixed() {
    my %h;
    my $bump = sub ($k) { %h{$k}++ };
    $bump('pre');
    return $bump, sub () { %h.elems };
}
my ($mb, $mc) = mixed();
$mb('post');
is $mc(), 2, 'pre-escape and post-escape bumps share one hash';

# threads: escaped closure incrementing under await
sub counter() {
    my @log;
    return sub ($v) { @log.push($v) }, sub () { @log.elems };
}
my ($add, $all) = counter();
await start { $add($_) for ^50 } xx 4;
is $all(), 200, 'escaped closure pushes from threads all land';

# two factory instances stay independent
my ($b1, $c1) = hash-bump();
my ($b2, $c2) = hash-bump();
$b1('k');
is $c2(), 0, 'independent factory instances do not share';
is $c1(), 1, 'first instance keeps its own count';
