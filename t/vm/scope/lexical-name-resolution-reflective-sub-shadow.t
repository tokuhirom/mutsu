use Test;

# A mainline sub that names a lexical by string (`EVAL`, `::('$x')`) reads the
# binding visible where the sub is declared, not a same-named `my` of the block
# that calls it (#13604, ADR-12529 phase 3). Its own `my` and parameters still
# come first. Writes through `::('$x') = v` and `EVAL` reach the same binding.
plan 13;

my $q = 'top';
sub via-eval { EVAL '$q' }
sub via-symbolic { ::('$q') }
sub via-both { $q.defined && ::('$q') }
sub own-my { my $q = 'own'; EVAL '$q' }
sub own-param($q) { ::('$q') }
sub sym-write { ::('$q') = 'sym-write' }
sub eval-write { EVAL '$q = "eval-write"' }

if True {
    my $q = 'blk';
    is via-eval(), 'top', 'EVAL in a mainline sub skips the calling block\'s $q';
    is via-symbolic(), 'top', 'so does ::(\'$q\')';
    is via-both(), 'top', 'and ::(\'$q\') in a sub that also reads $q directly';
    is own-my(), 'own', 'the sub\'s own my comes first';
    is own-param('param'), 'param', 'and so does its parameter';
}
{
    my $q = 'blk2';
    is via-eval(), 'top', 'a bare block shadow';
}
for 1 {
    my $q = 'loop';
    is via-symbolic(), 'top', 'a loop body shadow';
}
if True {
    my $q = 'blk3';
    sym-write();
    is $q, 'blk3', '::(\'$q\') = v leaves the calling block\'s $q alone';
}
is $q, 'sym-write', '...and writes the mainline $q';
if True {
    my $q = 'blk4';
    eval-write();
    is $q, 'blk4', 'EVAL\'s write leaves the calling block\'s $q alone';
}
is $q, 'eval-write', '...and writes the mainline $q';
$q = 'live';
is via-eval(), 'live', 'a later mainline write is seen';
is own-my(), 'own', 'own my after the writes';
