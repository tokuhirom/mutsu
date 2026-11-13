use Test;

# `Code.static_id` names the code an object runs: every closure clone of one
# block and every handle on one routine share it, and different code does not
# (Red's compiled-callable guard compares it, t/124).

plan 7;

sub f { }
sub k { }
my &g = &f;
isa-ok &f.static_id, Int, 'an Int';
ok &f.static_id == &g.static_id, 'every handle on a routine shares it';
ok &f.static_id != &k.static_id, 'another routine has its own';
ok &f.clone.static_id == &f.static_id, 'a clone keeps it';
my @blocks = (^2).map({ -> { $_ } });
ok @blocks[0].static_id == @blocks[1].static_id, 'closures of one block share it';
sub maker { sub { 1 } }
ok maker().static_id == maker().static_id, 'so do closures of one inner sub';
my $c = { 1 };
my $d = { 2 };
ok $c.static_id != $d.static_id, 'another block has its own';
