use Test;

# From the VERS distribution: `if EXPR -> @x` binds like an `@` parameter,
# so a Seq condition is cached into a List rather than rejected.
plan 5;

my $r = "a|b";
if $r.split("|", :skip-empty) -> @c {
    is @c.elems, 2, 'Seq condition bound to -> @c';
    is @c.List.raku, '("a", "b")', 'bound value is a List';
    is @c.elems, 2, 'readable twice';
}
my @arr = 1, 2, 3;
if @arr -> @p { is @p.elems, 3, 'Array condition still binds' }
dies-ok { if 5 -> @n { } }, 'non-Positional still fails';
