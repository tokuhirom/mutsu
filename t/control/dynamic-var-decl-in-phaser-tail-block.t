use v6.e.PREVIEW;
use Test;

# From the RedFactory distribution (Red::Do's `red-do`): a `my $*x` in a
# tail-position bare block that carries phasers (KEEP/UNDO/LEAVE/...) is a
# new lexical scope, so an earlier read of `$*x` in the enclosing routine
# body is not an X::Dynamic::Postdeclaration.

plan 3;

my @log;
sub g {
    @log.push("read") if $*FLAG;
    {
        my $*FLAG = True;
        LEAVE @log.push("leave");
        @log.push("in " ~ $*FLAG);
    }
}
lives-ok { g }, 'dynamic var declared in a phaser tail block compiles and runs';
is-deeply @log, ["in True", "leave"], 'phaser ran, outer read was falsy';

sub h {
    return 1 if $*G;
    {
        my $*G = 7;
        KEEP @log.push("keep");
        $*G
    }
}
is h(), 7, 'value of the tail block is returned';
