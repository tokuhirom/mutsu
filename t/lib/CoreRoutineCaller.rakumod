# Fixture for t/modules/compunit/routine-lookup-lexical-to-compunit.t (#17433):
# a module whose routines call core routines by name, and run callbacks.
unit module CoreRoutineCaller;

sub module-item is export { item [1, 2] }
sub module-any is export { so 'a' eq any(<a b>) }
sub run-it(&f) is export { f() }
sub run-method($o) is export { $o.go }
