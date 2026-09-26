use Test;

# A `return` inside a `whenever` of an on-demand `supply { }` block leaves the
# routine that tapped the supply, whether it taps through `react` or `.list`
# (#9630). The whenever is created while the supply block runs, so its
# `return` must target the routine enclosing the block, not the block.

plan 6;

sub via-react {
    my $s = supply { whenever Supply.from-list(1, 2) { return $_ } };
    react whenever $s { flunk "no value reaches the react" }
    5
}
is via-react(), 1, 'return in a nested whenever unwinds through react';

sub via-react-pointy {
    my $s = supply { whenever Supply.from-list(3, 4) -> $x { return $x * 10 } };
    react whenever $s { }
    5
}
is via-react-pointy(), 30, 'pointy whenever body returns through react';

sub via-list {
    my $s = supply { whenever Supply.from-list(1, 2) -> $x { return $x } };
    $s.list;
    5
}
is via-list(), 1, 'return in a nested whenever unwinds through .list';

sub via-list-bare {
    my $s = supply { whenever Supply.from-list(7, 8) { return "got $_" } };
    my @l = $s.list;
    5
}
is via-list-bare(), 'got 7', 'bare whenever body returns through .list';

sub conditional {
    my $s = supply { whenever Supply.from-list(1, 2, 3) { return $_ if $_ == 2; emit $_ } };
    my @seen;
    react whenever $s { @seen.push: $_ }
    'fell through'
}
is conditional(), 2, 'return fires on the matching value only';

sub no-return {
    my $s = supply { whenever Supply.from-list(1, 2) { emit $_ * 2 } };
    $s.list.List
}
is-deeply no-return(), (2, 4), 'a whenever without return still emits';
