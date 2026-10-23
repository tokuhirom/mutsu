use v6;
use Test;

plan 8;

class Thing { }
class Holder {
    has $.rc;
    method set { $!rc := Thing }
    method via-dot { $.rc.^name }
    method via-self { self.rc.^name }
    method construct { $.rc.new.^name }
}
role Tagged { }

my $plain = Holder.new;
$plain.set;
is $plain.via-dot, "Thing", "a := bound type object reads through \$.attr";

my $mixed = Holder.new but Tagged;
$mixed.set;
is $mixed.via-dot, "Thing", "also on a but-mixin invocant";
is $mixed.via-self, "Thing", "self.attr agrees";
is $mixed.construct, "Thing", "and the type object can be constructed from it";
is $mixed.rc.^name, "Thing", "and from the outside";

my $other = Holder.new but Tagged;
$other.set;
is $other.via-dot, "Thing", "a second mixin value behaves the same";

is (Holder.new but Tagged).rc.defined, False, "an unbound attribute stays undefined";
is $mixed.via-dot eq $plain.via-dot, True, "plain and mixin invocants agree";
