use Test;

# A named sub declared in a routine, called back through `.map(&sub)` from a
# recursive call, reads the lexicals of the activation that declared it -- not
# the outer activation's (#12478). Every expectation was checked against
# rakudo.

plan 9;

sub bind-param($t, $n) {
    my $type := $t;
    sub show($x) { $x ~~ Int ?? "[" ~ bind-param("IN", 0) ~ "]" !! "$type:$x" }
    $n ?? (1, "e").map(&show).join(",") !! ("e",).map(&show).join(",")
}
is bind-param("OUT", 1), '[IN:e],OUT:e', 'a `:=`-bound parameter is per activation';

sub param-after-bind($t, $n) {
    my $type := $t;
    sub cb($x) { param-after-bind("IN", 0) }
    $n ?? (1,).map(&cb).join !! "t=$t"
}
is param-after-bind("OUT", 1), 't=IN', 'binding a parameter does not adopt a caller\'s same-named cell';

sub direct-read($t, $n) {
    my $type := $t;
    sub show2($x) { $x ~~ Int ?? "[" ~ direct-read("IN", 0) ~ "]" !! "$type:$x" }
    $n ?? (1, "e").map(&show2).join !! "direct=$type " ~ show2("e")
}
is direct-read("OUT", 1), '[direct=IN IN:e]OUT:e', 'the inner activation reads its own `:=`-bound lexical';

class Node {
    has $.t;
    has @.kids;
    method tree {
        (<x>,).map(-> $name {
            my $type := $!t;
            sub show3($x) { $x ~~ Node ?? "[" ~ $x.tree ~ "]" !! "$type:$x" }
            (self.kids.Slip, "end").map(&show3).join(",")
        }).join
    }
    method flat-tree {
        my $type := $!t;
        sub show4($x) { $x ~~ Node ?? "[" ~ $x.flat-tree ~ "]" !! "$type:$x" }
        (|@!kids, "end").map(&show4).join(",")
    }
}

my $in = Node.new(t => "IN", kids => ());
my $root = Node.new(t => "OUT", kids => ($in, "leaf"));
is $root.tree, '[IN:end],OUT:leaf,OUT:end', 'a sub in a recursive `.map` callback reads its own activation (#12478)';
is $root.flat-tree, '[IN:end],OUT:leaf,OUT:end', 'the same without the callback block';
is $in.t, 'IN', 'binding `$!t` in the outer activation leaves the inner invocant\'s attribute alone';

sub deep($n) {
    my $level := $n;
    sub walk($x) { $x > 0 ?? deep($x - 1) ~ ",$level" !! "$level" }
    ($n,).map(&walk).join
}
is deep(3), '0,1,2,3', 'every recursion level keeps its own bound lexical';

my %kinds is Map = (a => Str);
sub per-iteration() {
    <a b>.map(-> $name {
        my $kind := %kinds{$name};
        sub kind-of($v) { $kind =:= Nil ?? 'nil' !! $kind.^name }
        $name => (1,).map(&kind-of).List
    }).List
}
is per-iteration().raku, '(:a(("Str",)), :b(("nil",)))', 'a Nil-bound lexical is per loop iteration for a sub declared there';

role Tagged[$tag] {
    method tagged($x) { (1,).map(-> $n { "$tag:$x" }).join }
}
class TaggedNode does Tagged['role-param'] { }
sub call-tagged { my $tag = 'caller'; TaggedNode.new.tagged('v') }
is call-tagged(), 'role-param:v', 'a role method still sees its role\'s parameters';

done-testing;
