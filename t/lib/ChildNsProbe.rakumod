# Records the HOW its own method traits see through `$*PACKAGE`.
our @seen;

multi sub trait_mod:<is>(Method:D \meth, :$probe!) is export {
    @seen.push: $*PACKAGE.HOW.^name;
}

unit class ChildNsProbe;

method write is probe(1) { 'written' }

method seen { @seen }
