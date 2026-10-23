use Test;

# `.^set_name` renames a role composition through its composition-keyed
# `.WHAT` node (ADR-0060), which every thread spawned after the node was made
# shares. NativeCall's `CArray[T]` / `Pointer[T]` `^parameterize` renames it
# on every call, so threads parameterizing one type at once wrote that node
# concurrently, and the unlocked insert crashed Cro's first connection with
# SIGSEGV (#14765). These programs synchronize nothing themselves: the node is
# mutsu's own state, so they must be memory-safe and read every name back
# (ADR-12814 §2.1).

plan 5;

role R { }
my @bases = (^200).map: { Metamodel::ClassHOW.new_type(:name("C$_")).^compose };
# Make every composition's shared node on this thread, before any worker
# exists, the way loading Cro::TCP::NoDelay makes `CArray[int32]`'s.
.^mixin(R).^name for @bases;

my $go = Promise.new;
my @workers = (^6).map: -> $t {
    start {
        await $go;
        my $seen = 0;
        for ^@bases -> $k {
            my $w := @bases[$k].^mixin(R);
            $w.^set_name("C$k+R");
            $seen++ if $w.^name eq "C$k+R";
        }
        $seen
    }
};
$go.keep;
is ([+] await @workers), 6 * @bases, 'every thread reads back each concurrent rename';

is @bases[7].^mixin(R).^name, 'C7+R', 'a rename made on a worker is visible on the main thread';
my $renamed := @bases[9].^mixin(R);
$renamed.^set_name('Nine');
is $renamed.raku, 'Nine', 'a renamed type object renders by its new name';

use NativeCall;
my @types = uint8, int16, int32, int64, num32, num64, Str;
my @names = flat @types.map: { CArray[$_].^name, Pointer[$_].^name };
$go = Promise.new;
@workers = (^6).map: -> $t {
    start {
        await $go;
        my @got;
        for ^30 {
            @got = flat @types.map: { CArray[$_].^name, Pointer[$_].^name };
        }
        @got.join(',')
    }
};
$go.keep;
is-deeply (await @workers).unique.List, (@names.join(','),),
    'NativeCall types parameterized on many threads at once keep their names';
is CArray[int32].new(1).elems, 1, 'and still construct (Cro::TCP::NoDelay)';
