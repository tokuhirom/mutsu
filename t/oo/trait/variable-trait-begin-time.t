use v6;
use Test;

# A `trait_mod:<is>(Variable ...)` handler runs while the declaring block is
# compiled (ADR-0134, #12278), so it sees the state left by the BEGIN written
# ahead of the declaration, not the state left by the last BEGIN of the unit.

plan 6;

my @seen;
multi sub trait_mod:<is>(Variable $v, :$env) {
    $v.var = %*ENV{$v.name.substr(1).uc} // 'none';
}

sub run(&c) { c() }

run({
    BEGIN { %*ENV = { "ATTRIBUTE" => "Here" }; }
    my $attribute is env;
    @seen.push($attribute);
});
run({
    BEGIN { %*ENV = { "ATTRIBUTE" => "There" }; }
    my $attribute is env;
    @seen.push($attribute);
});
is-deeply @seen, ['Here', 'There'], 'each declaration sees the BEGIN written before it';

# The trait value is the variable's starting value on every entry.
multi sub trait_mod:<is>(Variable $v, :$counted) {
    $v.var = 7;
}
sub counted-sub {
    my $x is counted;
    $x
}
is counted-sub(), 7, 'the trait value is the variable\'s starting value';
is counted-sub(), 7, 'a second entry starts from the same value';

# A scalar assigned after the declaration keeps its assignment.
BEGIN %*ENV = { "Y" => "from-env" };
sub assigned {
    my $y is env;
    my $before = $y;
    $y = 'x';
    ($before, $y)
}
is-deeply assigned(), ('from-env', 'x'), 'run-time assignment overrides the BEGIN-time value';

# A trait argument is evaluated for the declaration.
multi sub trait_mod:<is>(Variable $v, :$tagged) {
    $v.var = "tag-$tagged";
}
sub tagged-sub { my $t is tagged('a'); $t }
is tagged-sub(), 'tag-a', 'trait argument reaches the handler';

# A block entered twice gets a fresh variable each time.
sub fresh {
    my $z is tagged('b');
    $z ~= '!';
    $z
}
is-deeply (fresh(), fresh()), ('tag-b!', 'tag-b!'), 'each entry starts from the static value';
