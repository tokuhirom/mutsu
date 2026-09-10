use Test;

# `|EXPR` spreads the CONTAINER it is applied to, one level. A Capture that
# happens to be an element of that container is one argument, not a nested
# argument list -- `|` was applied to the array, not to the Capture. mutsu used
# to re-spread it, so XML's `make-xml('test', \('hello', :lang<en>, 'world'))`
# lost the nesting: `craft($name, |@contents)` handed `craft-new` the Capture's
# contents rather than the Capture, and its `$what ~~ Capture` arm never fired.

plan 8;

sub probe(*@pos, *%named) { (@pos.raku, %named.raku).join(' | ') }

{
    my $c = \('x', :k<v>, 'y');
    is probe(|$c), '["x", "y"] | {:k("v")}',
        '|$capture spreads its own lanes';
    is probe(|[$c]), '[\\("x", "y", :k("v"))] | {}',
        '|@array hands a Capture element over whole';
    is probe(|($c,)), '[\\("x", "y", :k("v"))] | {}',
        'and so does a slipped list';
}

# The relay shape XML uses: a slurpy re-splatted into another slurpy.
{
    sub inner(Str $n, *@c, *%a) { ($n, @c.raku, %a.raku).join(' | ') }
    sub outer(Str $n, *@contents, *%attribs) { inner($n, |@contents, |%attribs) }
    is outer('test', :type<embedded>, \('hello', :lang<en>, 'world')),
        'test | [\\("hello", "world", :lang("en"))] | {:type("embedded")}',
        'a Capture survives being relayed through two slurpy hops';
}

# The other slip shapes are unchanged.
{
    my @a = 1, 2;
    is probe(|@a), '[1, 2] | {}', '|@array spreads its elements';
    my %h = z => 9;
    is probe(|%h), '[] | {:z(9)}', '|%hash mints named arguments';
    is probe(|(3, 4)), '[3, 4] | {}', '|(list) spreads';
    my @p = (x => 1),;
    is probe(|@p), '[:x(1)] | {}', 'a Pair element of a slipped array stays positional';
}
