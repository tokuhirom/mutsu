use Test;

# A user `trait_mod:<is>` runs when its declaration is compiled, so the
# argument sees the unit's lexicals in their static state: declared, with no
# run-time initializer applied yet (ADR-0134). Checked against `raku` first.

plan 8;

my $seen;
my $shade = 7;
multi sub trait_mod:<is>(Mu:U $target, :$paint!) { $seen = $paint.raku }
class PhaseClass is paint($shade) { }
is $seen, 'Any', 'a class trait argument sees the static lexical';
is $shade, 7, 'the initializer still runs at its position';

my @log;
my $n = 3;
multi sub trait_mod:<is>(Mu:U $target, :$note!) { @log.push($note.raku) }
role PhaseRole is note($n) { }
module PhaseModule is note($n) { }
package PhasePackage is note($n) { }
is @log.join(','), 'Any,Any,Any', 'role, module and package arguments are static too';

our $pk-seen;
our $pk = 5;
multi sub trait_mod:<is>(Mu:U $target, :$pk-trait!) { $pk-seen = $pk-trait.raku }
class OurClass is pk-trait($pk) { }
is $pk-seen, 'Any', 'an our variable initializer has not run either';
is $pk, 5, 'and it is assigned afterwards';

my $order;
multi sub trait_mod:<is>(Mu:U $target, :$tag!) { $order ~= "t$tag" }
BEGIN $order ~= 'b';
class Ordered is tag(1) { }
is $order, 'bt1', 'the trait runs once, in source order with BEGIN';

is EVAL(q:to/END/), 'Any', 'string EVAL runs the trait in its declaration phase';
    my $r;
    my $v = 9;
    multi sub trait_mod:<is>(Mu:U $target, :$ev!) { $r = $ev.raku }
    class InEval is ev($v) { }
    $r;
    END

class Early is paint($shade) { }
is $seen, 'Any', 'a second use of the same trait sees the static value too';
