use v6;
use Test;

# `@!a = @e` / `%!h = %e` copies: the attribute gets its own container, so a
# later mutation through the attribute leaves the source alone. The store
# shared the source's container, and `@!a.push` grew `@e` too
# (Template::HAML's `Tag` BUILD, #10638).

plan 8;

class T {
    has @.a;
    has %.h;
    has @.src;
    method assign-push(@e) { @!a = @e; @!a.push: 'x'; @e.elems }
    method assign-index(@e) { @!a = @e; @!a[3] = 'x'; @e.elems }
    method assign-then-other(@e) { @!a = @e; self.grow; @e.elems }
    method grow { @!a.push: 'y' }
    method bind-push(@e) { @!a := @e; @!a.push: 'x'; @e.elems }
    method hash-assign(%e) { %!h = %e; %!h<new> = 1; %e.elems }
    submethod BUILD(:@attrs) {
        @!a   = @attrs.list;
        @!src = @attrs.list;
        @!a.push: 'class' => 'lead';
    }
}

my @e = 1;
is T.new.assign-push(@e), 1, 'push through the attribute leaves the source';
@e = 1;
is T.new.assign-index(@e), 1, 'index store through the attribute leaves the source';
@e = 1;
is T.new.assign-then-other(@e), 1, 'push from another method leaves the source';
@e = 1;
is T.new.bind-push(@e), 2, 'a := bind still aliases';
my %e = a => 1;
is T.new.hash-assign(%e), 1, 'hash attribute assignment copies';

my $t = T.new;
is $t.src.elems, 0, 'two attributes assigned from one list are independent';
is $t.a.elems, 1, 'the mutated attribute has its element';
is T.new(attrs => [5]).src.elems, 1, 'with a supplied list too';
