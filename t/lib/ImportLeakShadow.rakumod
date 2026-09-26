use ImportLeakPreUnit;
sub leak-ex($x) { "H" }
sub leak-shadow is export { leak-ex(1) }
