my proto sub tj(|) {*}
my multi sub tj(@_, *%_) { "[" ~ @_.map({ tj2 $_, :!pretty, |%_ }).join(",") ~ "]" }
sub tj2($x, :$pretty = True, *%o) is export { "<$x>" }
BEGIN &tj.add_dispatchee(&tj2);
sub EXPORT() { Map.new: ('&tj-out' => &tj,) }
