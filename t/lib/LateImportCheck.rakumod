
# Reads `$*CU` while the importing unit's own later `use` lines have not run
# yet, then inspects it at the unit's CHECK phase.
my @seen;

sub EXPORT() {
    my $cu = $*CU;
    $cu.add-check-code({
        sub walk($node) {
            return unless $node ~~ RakuAST::Node;
            if $node ~~ RakuAST::Call::Name && $node.name.canonicalize eq 'late-routine' {
                @seen.push: $node.is-resolved;
                @seen.push: $node.resolution.^name;
                @seen.push: $node.resolution.compile-time-value.name;
            }
            $node.visit-children(-> $child { walk($child) });
        }
        walk($cu);
    });
    Map.new('&late-seen' => sub { @seen })
}

unit module LateImportCheck;
