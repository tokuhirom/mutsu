use Test;

plan 4;

# A Bool on the smartmatch RHS IS the result, regardless of the topic — also
# when the topic is a type object. The Package-LHS catch-all returned False
# for `TypeObject ~~ True`, so Cro::CompositeConnector's BUILD (`when
# $seen-connector` over type-object components) classified every post-
# connector component as a "before" transform.

class T { }

ok (T ~~ True), 'type object ~~ True matches';
nok (T ~~ False), 'type object ~~ False fails';

my $seen = False;
my @before;
my @after;
role Marker { }
class Conn does Marker { }
class T1 { }
class T2 { }
for T1, Conn, T2 {
    when Marker { $seen = True; }
    when $seen { @after.push($_); }
    default { @before.push($_); }
}
is-deeply @before, [T1], 'pre-marker type object lands in before';
is-deeply @after, [T2], 'post-marker type object lands in after (when $bool matched)';
