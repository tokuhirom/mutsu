use Test;

# Pin for the (B) per-store env-write gate closure-capture cluster fix
# (docs/lexical-scope-slot-campaign.md, "(B) per-store env-write gate — burndown",
# closure-capture cluster).
#
# Three roots, all surfacing only when MUTSU_GATE_LOCAL_ENV_WRITE skips a plain
# lexical's env mirror:
#   1. A closure handed to the .map/.grep slow loop reads a captured free var back
#      from the creating frame's env by name (the loop pre-inserts the captured env
#      only for keys absent there). Fixed by folding nested-closure free vars into
#      needs_env_sync under the gate.
#   2. A `state` var in a closure is persisted at frame exit by reading its value;
#      that read now prefers the live slot (env can be stale).
#   3. A named sub reads an enclosing lexical by name from the defining frame's env;
#      fixed by keeping a sub-defining frame's locals env-synced under the gate.
# All pass gate-OFF (default) and would fail gate-ON before the fix.

plan 8;

# Root 1: map/grep closures capture an outer local.
sub mapgrep() {
    my $mul = 3;
    (1, 2, 3).map({ $_ * $mul }).grep({ $_ > 3 }).join(",");
}
is mapgrep(), "6,9", "map/grep blocks capture outer local";

sub mappointy() {
    my $add = 10;
    (1, 2, 3).map(-> $x { $x + $add }).join(",");
}
is mappointy(), "11,12,13", "pointy map block captures outer local";

# Root 2: state var in a closure persists and accumulates.
my $c = -> { state $s = 0; $s = $s + 10; $s };
is $c(), 10, "state var first call";
is $c(), 20, "state var persists across calls";
is $c(), 30, "state var keeps accumulating";

# Root 3: named subs read an enclosing (mainline) lexical.
my $base = 100;
sub f1() { f2() }
sub f2() { f3() }
sub f3() { $base + 1 }
is f1(), 101, "three-deep named sub chain reads outer lexical";

my $g = 5;
sub reads-g() { $g * 2 }
is reads-g(), 10, "named sub reads a mainline lexical";

# Combined: a named sub whose body maps a closure over an outer local.
my $factor = 4;
sub scale-list(@xs) { @xs.map({ $_ * $factor }).join(",") }
is scale-list((1, 2, 3)), "4,8,12", "named sub + map closure over outer local";
