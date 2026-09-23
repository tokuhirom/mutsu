module CustomExportTaggedStash {
    our sub selected is export(:SUPPORTED) { 'selected' }
}

sub EXPORT(*@args) {
    Map.new: |(EXPORT::SUPPORTED::{ @args.map: '&' ~ * }:p)
}
