use v6;
use Test;

# Two independent fixes exercised here:
#
# 1. `my %h is Some::Qualified::Class` — the variable-trait name parser must
#    keep the FULL `::`-qualified class name (a plain `ident` parser truncated
#    `Foo::Bar` to `Foo`, so tied-hash backing never found the class).
#
# 2. A class that supplies the associative protocol (STORE / AT-KEY) but does
#    NOT declare `does Associative` must still back a `%` variable as a tied
#    hash. Raku ties ANY class named by `is` on a `%` variable — the sigil
#    provides the associative semantics.

plan 8;

# A plain (non-role, no `does Associative`) tied-hash class with a qualified
# name. Backing store is an ordinary Hash so we avoid nqp ops.
class My::Tied::Hash {
    has %!store;
    method new() { self.bless }
    method STORE(::?CLASS:D: \pairs, :$INITIALIZE) {
        %!store = ();
        for pairs.list -> $p {
            %!store{$p.key} = $p.value;
        }
        self
    }
    method AT-KEY(::?CLASS:D: $key) is raw { %!store{$key} }
    method ASSIGN-KEY(::?CLASS:D: $key, \value) { %!store{$key} = value }
    method EXISTS-KEY(::?CLASS:D: $key) { %!store{$key}:exists }
    method keys(::?CLASS:D:) { %!store.keys }
}

my %h is My::Tied::Hash = a => 1, b => 2;

is %h.^name, 'My::Tied::Hash', 'qualified class name kept (not truncated to "My")';
is %h<a>, 1, 'tied AT-KEY reads initializer value (1)';
is %h<b>, 2, 'tied AT-KEY reads initializer value (2)';
is-deeply (%h<a>:exists), True, 'tied EXISTS-KEY for existing key';
is-deeply (%h<z>:exists), False, 'tied EXISTS-KEY for missing key';

%h<c> = 3;
is %h<c>, 3, 'tied ASSIGN-KEY stores a new key';

# A single-segment plain class with the protocol but no `does Associative`.
class Plainish {
    has %!s;
    method new() { self.bless }
    method STORE(\pairs) { for pairs.list -> $p { %!s{$p.key} = $p.value }; self }
    method AT-KEY($k) is raw { %!s{$k} }
}
my %p is Plainish = x => 10;
is %p.^name, 'Plainish', 'plain class (no Associative) still backs the variable';
is %p<x>, 10, 'plain tied class reads value';
