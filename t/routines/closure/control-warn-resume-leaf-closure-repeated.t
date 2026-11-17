use Test;

# A resumed CONTROL handler run inline at a warn raised inside a leaf closure
# (no call opcode) writes the installing frame's lexicals; the closure's return
# path must still carry the write back, on every call, not only the first.
# The write is tracked by a generation counter (`ControlState`), checked
# against its value at closure entry (ADR-10779 D3). Outputs checked against
# rakudo.

plan 3;

my $n = 0;
sub f(&c) { c() }
{
    CONTROL { when CX::Warn { $n++; .resume } }
    for ^3 { f({ 'x' x Int }) }
}
is $n, 3, 'every call of a leaf closure carries the handler write back';

my $flag = False;
{
    CONTROL { when CX::Warn { $flag = True; .resume } }
    my &leaf = { 'y' x Int };
    leaf();
}
ok $flag, 'a directly called leaf closure carries it back too';

my $count = 0;
{
    CONTROL { when CX::Warn { $count = $count + 1; .resume } }
    my &leaf = { 'z' x Int };
    leaf() for ^4;
}
is $count, 4, 'repeated calls see each write';
