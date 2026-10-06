use v6;
use Test;

# A source quit reaches every derived stage. `tail` discards its held values
# on quit, unlike done where it flushes them.
plan 10;

sub run-quit(&build) {
    my $s = Supplier.new;
    my @got;
    my @quit;
    build($s.Supply).tap({ @got.push($_) }, quit => { @quit.push(.message) });
    $s.emit(1);
    $s.emit(2);
    $s.quit(X::AdHoc.new(payload => 'boom'));
    (@got.List, @quit.List);
}

my ($got, $quit);

($got, $quit) = run-quit({ .map(* + 1) });
is-deeply $got, (2, 3), 'map emits values before quit';
is-deeply $quit, ('boom',), 'map forwards quit to its tap';

($got, $quit) = run-quit({ .grep(* > 0) });
is-deeply $got, (1, 2), 'grep emits matching values before quit';
is-deeply $quit, ('boom',), 'grep forwards quit to its tap';

($got, $quit) = run-quit({ .head(3) });
is-deeply $got, (1, 2), 'head emits values before the source quits';
is-deeply $quit, ('boom',), 'head forwards quit to its tap';

($got, $quit) = run-quit({ .tail(2) });
is-deeply $got, (), 'tail drops buffered values when the source quits';
is-deeply $quit, ('boom',), 'tail forwards quit to its tap';

($got, $quit) = run-quit({ .map(* + 1).grep(* > 0) });
is-deeply $got, (2, 3), 'a two-stage chain emits values before quit';
is-deeply $quit, ('boom',), 'a two-stage chain forwards quit to its tap';
