use Test;

# A destructive non-list `s///` that matches nothing evaluates to `Nil`
# (the failed match), while `$x ~~ s///` reports the failure as `False`
# (issue #9515).

plan 13;

$_ = "abc";
is (s/q/b/).raku, 'Nil', 'failed s/// is Nil';
{
    my $r = s/q/b/;
    is $r.raku, 'Any', 'failed s/// assigned to a scalar reads back as Any';
}
nok (s/q/b/).defined, 'failed s/// is undefined';
is (s/q/b/ // 'd'), 'd', 'failed s/// falls through //';
nok (so s/q/b/), 'failed s/// is falsy';
is (s:nth(1)/q/b/).raku, 'Nil', 'failed single-:nth s/// is Nil';
isa-ok (s:g/q/b/), List, 'failed s:g/// is still a List';
is (s:g/q/b/).elems, 0, '... and it is empty';

my $x = "abc";
is ($x ~~ s/q/b/).raku, 'Bool::False', 'failed $x ~~ s/// is False';
is ($x !~~ s/q/b/).raku, 'Bool::True', 'failed $x !~~ s/// is True';
isa-ok ($x ~~ s/a/z/), Match, 'successful $x ~~ s/// is the Match';
is $x, 'zbc', '... and it wrote the variable';

$_ = "abc";
isa-ok (s/a/z/), Match, 'successful s/// is the Match';
