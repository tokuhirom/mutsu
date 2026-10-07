class ExportTermHolder { method greet { "hello" } }

sub t is export(:t) { ExportTermHolder.new }

sub EXPORT($t = 't') { %( $t => ExportTermHolder.new ) }
