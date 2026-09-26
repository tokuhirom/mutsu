sub leak-ex($x) { "T" }
sub leak-private-call is export { leak-ex(1) }
