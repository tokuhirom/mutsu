sub EXPORT(*@names) {
    Map.new('&inner-thing' => sub { 'from inner custom EXPORT' })
}
unit module InnerCustomExport;
