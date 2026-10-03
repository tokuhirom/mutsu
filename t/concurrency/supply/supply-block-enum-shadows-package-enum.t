use Test;

plan 4;

# An enum declared inside a supply block binds its keys lexically: a
# `whenever` callback sees them, not a same-named key of another enum
# (TAP's `enum Mode <Normal SubTest Yaml>` vs `enum Formatter::Volume`).
enum Volume (:Silent(-2) :Normal(0));

sub ps($in) {
    supply {
        enum Mode <Normal Other>;
        my Mode $mode = Normal;
        whenever $in -> $l { emit ($mode === Normal, Normal.^name) }
    }
}
is-deeply ps(Supply.from-list(1)).list, ((True, 'Mode'),), 'whenever sees the block enum key';
is Normal.^name, 'Volume', 'the outer key is untouched';

# Same-named keys in different routines' scopes do not poison each other.
sub f { enum M2 <Normal X>; my $c = { Normal }; $c }
is f()().^name, 'M2', 'a closure over a routine-local enum key';
lives-ok { Normal }, 'no poisoned alias across scopes';
