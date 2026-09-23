sub exported is export(:all) { 'from lowercase all' }

sub EXPORT(*@args, *%_) {
    Map.new( |(EXPORT::all::{ @args.map: '&' ~ * }:p) )
}
