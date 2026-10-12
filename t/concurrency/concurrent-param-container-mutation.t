use Test;

# A container mutated from several `start` blocks that reach it only through a
# routine parameter (#11210). Unsynchronized concurrent mutation is a user
# error in Raku and its result is unspecified (rakudo itself crashes on these
# shapes), so these pin MUTSU's guarantee, not a Raku one: the process ends in
# an answer, never a panic or a corrupted heap. Each reader shape below once
# located the array's live range unguarded while another thread's `shift` had
# it torn, and panicked with "range start index N out of range".

plan 9;

sub drive(&work) {
    my @a;
    await (^8).map: { start work(@a) };
    True
}

# The reader in the `if` is what differs between these.
ok drive(sub (@x) { for ^5000 { @x.push($_); @x.shift if @x > 10 } }),
    'numeric comparison of a shared parameter array';
ok drive(sub (@x) { for ^5000 { @x.push($_); @x.shift if +@x > 10 } }),
    'prefix numeric coercion of a shared parameter array';
ok drive(sub (@x) { for ^5000 { @x.push($_); @x.shift if @x } }),
    'boolean test of a shared parameter array';
ok drive(sub (@x) { for ^5000 { @x.push($_); @x.shift if @x.Bool } }),
    '.Bool of a shared parameter array';
ok drive(sub (@x) { for ^5000 { @x.push($_); @x.shift if @x.elems > 10 } }),
    '.elems of a shared parameter array';

sub drive-hash(&work) {
    my %h;
    await (^8).map: -> $i { start work(%h, $i) };
    True
}

ok drive-hash(sub (%y, $i) {
    for ^2000 -> $j { %y{"k$i-$j"} = [$j]; %y{"k$i-{$j-1}"}:delete }
}), 'insert and delete through a shared parameter hash';
ok drive-hash(sub (%y, $i) {
    my $n = 0;
    for ^2000 -> $j { %y{"k$i-$j"} = [$j]; %y{"k$i-{$j-1}"}:delete; $n++ if %y }
}), 'boolean test of a shared parameter hash';
ok drive-hash(sub (%y, $i) {
    my $n = 0;
    for ^2000 -> $j { %y{"k$i-$j"} = [$j]; %y{"k$i-{$j-1}"}:delete; $n++ if +%y > 3 }
}), 'numeric coercion of a shared parameter hash';

# Sequential use of the same routine shape still answers exactly.
my @solo;
sub fill(@x) { for ^100 { @x.push($_); @x.shift if @x > 10 } }
fill(@solo);
is-deeply @solo, [90..99], 'single-threaded, the same code answers exactly';
