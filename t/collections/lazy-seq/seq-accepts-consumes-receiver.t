use Test;
plan 9;

# Seq.ACCEPTS against a list-like topic walks both sides and so consumes the
# receiver (#13332); against any other topic it is an identity test.
{
    my $s = (1, 2, 3).Seq;
    ok $s.ACCEPTS((1, 2, 3)), 'first ACCEPTS of a list topic answers True';
    throws-like { $s.ACCEPTS((1, 2, 3)) }, X::Seq::Consumed, 'the receiver is consumed';
}
{
    my $a = (1, 2, 3).Seq;
    my $b = (1, 2, 3).Seq;
    ok $a.ACCEPTS($b), 'a Seq topic compares equal';
    throws-like { $a.ACCEPTS($b) }, X::Seq::Consumed, 'the receiver is consumed';
    throws-like { $b.List }, X::Seq::Consumed, 'and so is the Seq topic';
}
{
    my $d = (1, 2, 3).Seq;
    throws-like { $d.ACCEPTS($d) }, X::Seq::Consumed, 'a Seq cannot be walked against itself';
}
{
    my $e = (1, 2, 3).Seq;
    nok $e.ACCEPTS(5), 'a non-list topic is False';
    nok $e.ACCEPTS(5), 'and does not consume the receiver';
    is $e.List, (1, 2, 3), 'the Seq is still readable';
}
