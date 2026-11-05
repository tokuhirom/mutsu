use Test;

# Rakudo registers a variable's `will leave|keep|undo` traits on the scope
# apart from the LEAVE/KEEP/UNDO blocks: at exit every block runs first (last
# declared first), then the traits (last declared first).

plan 4;

my @log;

{
    LEAVE { @log.push: "L1" }
    my $a will leave { @log.push: "a" }
    LEAVE { @log.push: "L2" }
    my $b will leave { @log.push: "b" }
    LEAVE { @log.push: "L3" }
    @log.push: "body";
}
is @log.join(" "), "body L3 L2 L1 b a", "bare block: LEAVE blocks, then will leave traits";

@log = ();
sub f {
    LEAVE { @log.push: "L1" }
    my $a will leave { @log.push: "a" }
    LEAVE { @log.push: "L2" }
    my $b will leave { @log.push: "b" }
    @log.push: "body";
}
f();
is @log.join(" "), "body L2 L1 b a", "sub body: same order";

@log = ();
sub g {
    LEAVE { @log.push: "L1" }
    my $a will undo { @log.push: "undone" }
    LEAVE { @log.push: "L2" }
    my $b will keep { @log.push: "kept" }
    @log.push: "body";
    5
}
g();
is @log.join(" "), "body L2 L1 kept", "will keep runs after the LEAVE blocks, will undo not at all";

@log = ();
sub h {
    LEAVE { @log.push: "L1" }
    my $a will undo { @log.push: "undone" }
    LEAVE { @log.push: "L2" }
    @log.push: "body";
    Nil
}
h();
is @log.join(" "), "body L2 L1 undone", "will undo runs after the LEAVE blocks on a Nil exit";
