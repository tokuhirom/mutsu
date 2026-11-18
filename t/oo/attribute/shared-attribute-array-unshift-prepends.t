use Test;

# Once a second thread exists, an attribute array's `.unshift` still adds at
# the front. It took the shared-array push lane and came out as a push
# (Terminal::LineEditor's `@!char-widths-prefix-sum.unshift(0)` read
# `[1, 2, 3, 0]` after `CLIInput.new` had started its decoder thread;
# Rakuish t/58-feel-field.t).

plan 4;

my $bg = start { sleep 0.3 };

class Prefix {
    has @!p;
    method sums(*@w) { @!p = [\+] @w; @!p.unshift(0); @!p }
    method mixed { @!p = 1, 2, 3; @!p.unshift(0, 9); @!p.push(4); @!p.prepend(7); @!p }
}

is-deeply Prefix.new.sums(1, 1, 1), [0, 1, 2, 3], 'unshift onto a [\+] result';
is-deeply Prefix.new.mixed, [7, 0, 9, 1, 2, 3, 4], 'unshift, push and prepend keep their ends';

my @a;
await (^50).map: -> $i { start { @a.unshift($i) } };
is @a.elems, 50, 'concurrent unshifts onto a lexical array all land';

await $bg;
is-deeply Prefix.new.sums(2, 3), [0, 2, 5], 'after the thread ended too';
