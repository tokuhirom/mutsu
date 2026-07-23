use Test;

# Raku passes the `:INITIALIZE` named argument to a tied hash's `STORE` on the
# *declaration* assignment (`my %h is Foo = ...`) but NOT on a later `%h = ...`
# reassignment. A write-once tie relies on this to tell the initial population
# apart from a forbidden overwrite (the WriteOnceHash dist pattern).

plan 5;

my @seen;

my class Tied does Associative {
    has %.store;
    method STORE(\values, :$INITIALIZE) {
        @seen.push($INITIALIZE ?? 'init' !! 'reassign');
        %!store = ();
        for values.list -> $p { %!store{$p.key} = $p.value }
        self
    }
    method AT-KEY($k) { %!store{$k} }
    method keys { %!store.keys }
}

my %h is Tied = a => 1, b => 2;
is %h<a>, 1, 'declaration initializer stored (a)';
is %h<b>, 2, 'declaration initializer stored (b)';
is-deeply @seen, ['init'], 'declaration assignment passed :INITIALIZE';

%h = c => 3;
is %h<c>, 3, 'reassignment stored';
is-deeply @seen, ['init', 'reassign'], 'reassignment did NOT pass :INITIALIZE';
