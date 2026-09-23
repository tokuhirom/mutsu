module CustomExportBuiltinShadow {
    our sub any(&block, *@args) is export(:SUPPORTED) {
        'custom-any'
    }
}

sub EXPORT(*@args) {
    Map.new: |(EXPORT::SUPPORTED::{ @args.map: '&' ~ * }:p)
}
