# `$::Pkg::($name)` symbolic scalar lookup

`my $v = $::Date::Names::en::($n)` died with `Malformed initializer`: a scalar with a leading-`::`
package head followed by a `::(...)` symbolic tail was not parsed, although `$Pkg::($n)` and
`$::($n)` were. The scalar parser now treats `$::Pkg::($n)` like `$Pkg::($n)`. Found via the
`Date::Names` distribution (`t/5-en-class.t`, 49 assertions now pass).
