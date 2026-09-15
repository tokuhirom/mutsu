unit module ReExportTraitProvider;

role ReExportMarked {
    method re-export-mark-value { 'marked' }
}

multi sub trait_mod:<is>(Attribute $attr, :$re-export-mark!) is export {
    $attr does ReExportMarked;
}

sub re-export-greet() is export { 'hi' }
