use Test;

# A block has no `@_` of its own unless its signature declares one: a block
# inside a routine that declares `*@_` reads the routine's `@_`, however the
# block is called (#13990). Called directly (`{ @_.elems }()`) or through a
# variable, it read the empty argument list of its own call before. The
# implicit `@_` the parser gives a block that reads it still belongs to the
# innermost such block (roast S04-statements/if.t's `if 1, 2 { @_ }`).

plan 7;

sub direct(*@_) { { @_.elems }() }
is direct(1, 2, 3), 3, 'a directly called block reads the routine @_';

sub stored(*@_) { my &b = { @_.elems }; b() }
is stored(1, 2, 3), 3, 'so does a block called through a variable';

sub mapped(*@_) { (1,).map({ @_.elems }).head }
is mapped(1, 2, 3), 3, 'and one called by .map';

sub with-placeholder(*@_) { { $^a + @_.elems }(10) }
is with-placeholder(1, 2, 3), 13, 'a block with its own placeholder still reads the outer @_';

# TODO: the inner block's capture takes the middle block's frame, where the
# middle block's own call left an empty `@_`; the routine's `@_` behind it is
# flattened away (#14024; ADR-12529 phase 4 retires that capture).
todo 'a block two levels down still reads the middle block call\'s @_';
sub nested(*@_) { { { @_.join(',') }() }() }
is nested(1, 2), '1,2', 'a block nested in a block reads it too';

sub own(*@_) { -> *@_ { @_.elems }(9) }
is own(1, 2, 3), 1, 'a block that declares *@_ reads its own';

my @got;
{
    if 1, 2 { @got = @_ }
}
is-deeply @got, [1, 2], "an if block's implicit @_ is its own";
