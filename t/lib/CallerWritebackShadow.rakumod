# Fixture for t/vm/writeback/caller-writeback-not-claimed-by-deeper-frame.t:
# `runs-then-describes` calls the caller's block, then a routine whose
# `is copy` parameter shares the name of the variable that block wrote.
sub describe($desc is copy) { my $line = "ok - $desc"; $line }

sub runs-then-describes(&code, :$label) is export {
    code();
    describe($label // 'x')
}
