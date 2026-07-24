unit module EvalModHelper;

# A file-scoped helper the module's own exports call. It must stay reachable
# after a registry-restoring scope (`EVAL`, a bare block) that did the `use`
# has exited -- see t/eval-module-lexical-subs.t.
my sub helper($x) { $x * 2 }

my sub doubled($x) is export { helper($x) }
our sub tripled($x) is export { helper($x) + $x }
