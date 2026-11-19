use Test;

# A Seq held in a `$` container is one item when it lands in an `@`
# container, exactly as an itemized Array or List is (#18193): through a
# constructor's named argument, an rw accessor, and `append`/`prepend`.
# Destructuring (`my ($a, $b) = $seq`) and `splice` still iterate it.

plan 12;

class G { has @.a; has @.b is rw }
class H { has @.a; multi method new(Int $x) { self.bless } }

my $s = (1, 2).map({ $_ });
is G.new(a => $s).a.elems, 1, 'constructor: one element';
is-deeply G.new(a => $s).a[0].list, (1, 2), 'constructor: the element is the Seq';
is H.new(a => $s).a.elems, 1, 'default-new fallback: one element';
my $g = G.new; $g.b = $s;
is $g.b.elems, 1, 'rw accessor assignment: one element';
my @x = $s;
is @x.elems, 1, 'my @x = $seq: one element';

my @f; @f.append($s);
is @f.elems, 1, 'append($seq): one element';
my @p; @p.prepend($s);
is @p.elems, 1, 'prepend($seq): one element';
my @q; @q.append((1, 2).map({ $_ }));
is-deeply @q, [1, 2], 'append of a bare Seq still flattens';

my $d = (1, 2).map({ $_ });
my ($m, $n) = $d;
is-deeply ($m, $n), (1, 2), 'destructuring iterates the Seq';
my $e = (1, 2).map({ $_ });
my @i = 0; @i.splice(1, 0, $e);
is-deeply @i, [0, 1, 2], 'splice of a lone $seq inserts its elements';

is-deeply G.new(a => (1, 2).map({ $_ })).a, [1, 2], 'a bare Seq still flattens';
my $l = (1, 2);
is G.new(a => $l).a.elems, 1, 'an itemized List stays one element too';
