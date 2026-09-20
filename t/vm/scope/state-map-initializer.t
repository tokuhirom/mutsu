use Test;

plan 2;

sub cached-values() {
    state %values = map { $_ => $_ * 2 }, 1, 2, 3;
    %values
}

my %values = cached-values();
is %values<2>, 4, 'a state hash eagerly materializes a mapped sequence';
is-deeply %values.keys.sort, ('1', '2', '3'), 'the mapped keys are retained';
