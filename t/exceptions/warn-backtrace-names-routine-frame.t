use Test;
plan 3;

# An uncaught warn's location line names the routine it ran in (#13379), not
# always `block <unit>`; Rakudo prints that one innermost frame only.
class Trap {
    has $.buf is rw = "";
    method print(*@a) { $!buf ~= @a.join; True }
    method say(*@a) { $!buf ~= @a.join ~ "\n"; True }
    method flush { True }
}

sub in-sub { warn "from sub" }
class Warner { method check { warn "from method" } }

sub warning-of(&code) {
    my $*ERR = Trap.new;
    code();
    $*ERR.buf;
}

my $sub-warning = warning-of({ in-sub });
like $sub-warning, /'in sub in-sub at '/, 'a warn in a sub names the sub';
unlike $sub-warning, /'in block <unit>'/, 'and does not list the caller frames';

my $method-warning = warning-of({ Warner.check });
like $method-warning, /'in method check at '/, 'a warn in a method names the method';
