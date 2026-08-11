# A Slangify-shaped EXPORT generator: the outer EXPORT runs when a module
# does `use InnerExportProvider <name> <tag>` and returns a Map exporting an
# inner &EXPORT closure. That closure becomes the *using module's* own EXPORT,
# called (with the end user's `use` arguments) when the end user imports it.
sub EXPORT($name, $tag) {
    my sub EXPORT(*@args) {
        my $suffix = @args ?? "-" ~ @args.join("-") !! "";
        Map.new: ("&$name" => sub () { "provided-by-$tag$suffix" })
    }
    Map.new: ('&EXPORT' => &EXPORT)
}
