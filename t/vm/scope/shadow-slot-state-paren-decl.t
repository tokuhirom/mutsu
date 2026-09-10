use v6;
use Test;

# §1.3 S15: `(state @foo) = @bar` reassigns the state aggregate on EVERY call
# (unlike `state @foo = @bar`, which initializes once). The state aggregate
# lives in a shared ContainerRef cell aliased by the persisted state store;
# the parenthesized-declaration assignment must write THROUGH that cell, not
# replace the local slot (which detaches it from the store, so the next call
# restores the stale cell and the reassignment is lost). Pin for
# roast/S04-declarations/state.t #13 under MUTSU_SHADOW_SLOTS=1.
# Must pass with MUTSU_SHADOW_SLOTS unset (default) AND =1, and on real raku.

plan 5;

my @bar = 1, 2, 3;
sub swatest {
    (state @foo) = @bar;
    my $x = @foo.join('|');
    @foo[0]++;
    return $x;
}
is swatest(), '1|2|3', '(state @foo) = @bar assigns on first call';
is swatest(), '1|2|3', '(state @foo) = @bar reassigns on second call';
is swatest(), '1|2|3', '(state @foo) = @bar reassigns on third call';

my %src = a => 1;
sub hwatest {
    (state %h) = %src;
    my $x = %h<a>;
    %h<a> = 99;
    return $x;
}
is hwatest(), 1, '(state %h) = %src assigns on first call';
is hwatest(), 1, '(state %h) = %src reassigns on second call';
