my $imported;
{
    use ImportedDeclarationConstants;
    $imported = e;
}
sub EXPORT() {
    Map.new('e' => $imported)
}
