use Test;

# #19135: `|EXPR` in an argument list or a list flattens when the capture or
# list is built, after every argument has been evaluated, as in rakudo. A
# later argument that changes the slipped container is seen by the slip.

plan 16;

sub collect(*@a, *%h) { (@a, %h.sort.List) }

{
    my @p = 1, 2, 3;
    my ($pos, $named) = collect(|@p, :t(@p.pop));
    is-deeply $pos, [1, 2], 'a named argument that pops runs before the slip flattens';
    is-deeply $named, (t => 3,), '... and binds the popped element';
}
{
    my @p = 1, 2, 3;
    is-deeply collect(|@p, @p.pop)[0], [1, 2, 3], 'a positional argument after the slip';
}
{
    my @p = 1, 2, 3;
    is-deeply collect(:t(@p.pop), |@p)[0], [1, 2], 'a named argument before the slip runs first';
}
{
    my @p = 1, 2;
    my @r = 3, 4;
    is-deeply collect(|@p, |@r, @r.pop, @p.pop)[0], [1, 3, 4, 2], 'two slips, each mutated later';
}
{
    my @p = 1, 2, 3;
    is-deeply collect(|@p, 9)[0], [1, 2, 3, 9], 'a slip followed by a literal';
}
{
    my %h = a => 1;
    is-deeply collect(|%h, %h<b> = 2)[1], (a => 1, b => 2), 'a hash slip sees a later store';
}
{
    my @p = 1, 2, 3;
    is-deeply (|@p, @p.shift), (2, 3, 1), 'a list literal';
}
{
    my @p = 1, 2, 3;
    is-deeply [|@p, @p.pop, 5], [1, 2, 3, 5], 'an array literal';
}
{
    my @p = 1, 2, 3;
    my @q = |@p, @p.pop;
    is-deeply @q, [1, 2, 3], 'an array assignment';
}
{
    my @p = 1, 2, 3;
    my @a = 0;
    @a.push(|@p, @p.pop);
    is-deeply @a, [0, 1, 2, 3], 'a method call';
}
{
    my @p = 1, 2, 3;
    my &f = -> *@a { @a };
    is-deeply f(|@p, @p.pop), [1, 2, 3], 'a call through a code variable';
    @p = 1, 2, 3;
    is-deeply &f(|@p, @p.pop), [1, 2, 3], 'an &f(...) call';
}
{
    my @p = 1, 2, 3;
    my $c = class { method m(*@a, :$t) { (@a, $t) } };
    is-deeply $c.m(|@p, :t(@p.pop)), ([1, 2], 3), 'a method call on a type object';
}
{
    # TOML's `parse-date`: the timezone is popped off the parts it slips.
    my @part = 2001, 1, 1, 15, 33, 0, 8 * 3600;
    my $d = DateTime.new(|@part, :timezone(@part.pop));
    is $d.Str, '2001-01-01T15:33:00+08:00', 'DateTime.new(|@part, :timezone(@part.pop))';
}
{
    my $seen = 0;
    sub bump { $seen++; 7 }
    is-deeply collect(|(1, 2), bump()), ([1, 2, 7], ()), 'a slip of a literal list';
}
