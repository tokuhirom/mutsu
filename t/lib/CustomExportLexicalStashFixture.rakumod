sub EXPORT(*@names) {
    Map.new('&exported-by-lexical-stash' => sub { 'from custom EXPORT' })
}
unit module CustomExportLexicalStashFixture;
