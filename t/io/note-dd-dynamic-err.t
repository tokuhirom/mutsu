use Test;

# #19048: `dd` prints through the dynamic `$*ERR`, as `note` does, so a
# program or test that rebinds `$*ERR` captures its output.

plan 3;

my $out = '';
{
    my $*ERR = class {
        method print(*@a) { $out ~= @a.join }
        method say(*@a)   { $out ~= @a.join ~ "\n" }
    }.new;
    dd 42;
    dd 'x';
}
is $out, "42\n\"x\"\n", 'dd writes to the rebound $*ERR';

my $noted = '';
{
    my $*ERR = class {
        method print(*@a) { $noted ~= @a.join }
        method say(*@a)   { $noted ~= @a.join ~ "\n" }
    }.new;
    note 'n';
    dd 'd';
}
is $noted, "n\n\"d\"\n", 'note and dd go to the same handle, in order';

my $ast = '';
{
    my $*ERR = class {
        method print(*@a) { $ast ~= @a.join }
        method say(*@a)   { $ast ~= @a.join ~ "\n" }
    }.new;
    dd RakuAST::IntLiteral.new(42);
}
is $ast, "42\n", 'a RakuAST node is printed through the rebound $*ERR too';
