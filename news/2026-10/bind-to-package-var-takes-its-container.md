# `my $x := $Pkg::y` binds the package variable itself

Binding a lexical to a package-qualified scalar (`my $v := $Config::level`)
now aliases that package variable's own container. The bind used to look
its source up by name when it ran. A routine could then end up bound to a
different variable with the same short name, for example the program's own
`our $level`, after other blocks had bound `$level` lexically (#13941).
