use Test;

# §D state-ownership: the VM constructs the simple concurrency primitives
# (`Promise`, `Channel`, `Supplier`, `Supplier::Preserving`) natively (no
# interpreter `.new` bounce), behaving identically to the interpreter's
# `dispatch_new` arms.

plan 10;

# --- Promise ---
{
    my $p = Promise.new;
    isa-ok $p, Promise, 'Promise.new returns a Promise';
    $p.keep(42);
    is $p.result, 42, 'kept Promise yields its result';
    ok $p.status ~~ /Kept/, 'kept Promise status is Kept';
}

# --- Channel ---
{
    my $c = Channel.new;
    isa-ok $c, Channel, 'Channel.new returns a Channel';
    $c.send(7);
    $c.send(8);
    $c.close;
    is $c.list, (7, 8), 'Channel delivers sent values in order';
}

# --- Supplier ---
{
    my $s = Supplier.new;
    isa-ok $s, Supplier, 'Supplier.new returns a Supplier';
    my @got;
    $s.Supply.tap(-> $v { @got.push($v) });
    $s.emit(1);
    $s.emit(2);
    is @got, [1, 2], 'Supplier emits to a tapped Supply';
}

# --- Supplier::Preserving (construction + post-tap emission) ---
{
    my $s = Supplier::Preserving.new;
    isa-ok $s, Supplier::Preserving, 'Supplier::Preserving.new returns the type';
    my @got;
    $s.Supply.tap(-> $v { @got.push($v) });
    $s.emit("a");
    is @got, ["a"], 'Supplier::Preserving emits to a tapped Supply';
}

# --- a fresh Promise is not equal to another (distinct shared state) ---
{
    my $p1 = Promise.new;
    my $p2 = Promise.new;
    ok $p1 !=== $p2, 'two Promise.new are distinct';
}
