sub exported is export(:all) { 'from lowercase all' }
our constant &exported-alias is export(:all) = &exported;
our proto sub exported-multi(|) is export(:all) {*}
multi sub exported-multi(Int $value) { "int:$value" }
multi sub exported-multi(Str $value) { "str:$value" }

sub EXPORT(*@args, *%_) {
    Map.new( |(EXPORT::all::{ @args.map: '&' ~ * }:p) )
}
