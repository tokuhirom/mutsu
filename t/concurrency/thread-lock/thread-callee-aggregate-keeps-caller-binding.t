use Test;

# A callee's own `my %h` / `my @a` that spawned a thread must not replace the
# caller's same-named variable once the callee has returned (#15020; Red's
# `^create` declares `my %pars`, and its `save` emits through a `start`).

plan 6;

sub emit { start try { 1 } }

sub make-hash(*%orig) {
    my %pars;
    for %orig.kv -> $k, $v { %pars{$k} = $v }
    emit();
    %pars
}

my %pars = a => 5;
make-hash(num1 => 1);
%pars<z> = 1;
is-deeply %pars.keys.sort.List, <a z>, 'caller %hash keeps its keys after a spawning callee';

sub make-array(*@orig) {
    my @items;
    @items.push($_) for @orig;
    emit();
    @items
}

my @items = 9;
make-array(1, 2);
@items.push(10);
is-deeply @items, [9, 10], 'caller @array keeps its elements after a spawning callee';

sub nested(\depth, *%orig) {
    my %pars;
    for %orig.kv -> $k, $v {
        %pars{$k} = $v ~~ Associative ?? nested(depth + 1, |$v) !! $v;
    }
    emit();
    %pars
}

my $r = nested(0, name => "outer", child => %( name => "inner" ));
is $r<name>, "outer", 'a recursive frame keeps its own %hash across the nested spawning call';
is $r<child><name>, "inner", 'and the nested call keeps its own';

sub plain { my %pars = q => 1; %pars }
my %mine = a => 5;
plain();
%mine<z> = 1;
is-deeply %mine.keys.sort.List, <a z>, 'a callee without a spawn leaves the caller alone';

my %shared;
my $p = start { %shared<k> = 1 };
await $p;
is-deeply %shared.keys.List, ('k',), 'a hash the caller shares with its own thread still works';
