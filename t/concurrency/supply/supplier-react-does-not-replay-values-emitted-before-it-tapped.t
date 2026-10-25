use Test;

# A plain Supplier drops what is emitted while nobody has tapped it. A `react`
# that taps it later must not be handed those values; only what is emitted
# between its `whenever` and the end of its setup (a producer racing ahead of
# the sink registration) reaches it. Red 0.2.7 t/43-prefetch relies on this:
# `start react whenever Red.events { flunk if $++ }` is started after a hundred
# row events were emitted.

plan 5;

my $sup = Supplier.new;
my $sy  = $sup.Supply;
$sup.emit(1);
$sup.emit(2);
my @got;
react {
    whenever $sy -> $v { @got.push($v); done }
    $sup.emit(3);
}
is-deeply @got, [3], 'values emitted before the first react tapped are not replayed';

my $sup2 = Supplier.new;
my $sy2  = $sup2.Supply;
my @a;
react { whenever $sy2 -> $v { @a.push($v); done }; $sup2.emit(5); }
$sup2.emit(6);
my @b;
react { whenever $sy2 -> $v { @b.push($v); done }; $sup2.emit(7); }
is-deeply @a, [5], 'the first react saw its own value';
is-deeply @b, [7], 'a value emitted between two reacts is not replayed to the second';

# A producer thread that emits between the `whenever` and the end of the react
# body setup is still delivered (the race the buffer exists for).
my $sup3 = Supplier.new;
my $sy3  = $sup3.Supply;
$sup3.emit(0);
my @c;
react {
    whenever $sy3 -> $v { @c.push($v); done if @c == 3 }
    start { $sup3.emit($_) for 1..3 }
}
is-deeply @c, [1, 2, 3], 'a producer started inside the react is delivered in order';

my $started = Promise.new;
my $sup4 = Supplier.new;
$sup4.emit($_) for ^5;
my @d;
my $p = start react {
    whenever $sup4.Supply -> $v { @d.push($v); done if @d == 2 }
    $started.keep;
}
await $started;
$sup4.emit($_) for 10, 11;
await $p;
is-deeply @d, [10, 11], 'a react started after earlier emits only sees later ones';
