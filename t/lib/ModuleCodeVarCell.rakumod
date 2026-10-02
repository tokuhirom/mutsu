unit module ModuleCodeVarCell;

# #11051: a module-level `my &var` that an exported sub reassigns, read by
# closures the module mainline created at load time.
my %filters;
our sub register-filter(:$name!, :&handler!) is export { %filters{$name} = &handler }
our sub run-filter($name, $body) is export { %filters{$name}($body) }

my &backend;
our sub register-backend(:&handler!) is export { &backend = &handler }
our sub backend-registered() is export { &backend.defined }
register-filter :name<md>, :handler(-> $body { &backend.defined ?? backend($body) !! 'MISSING' });

my &initial = -> $x { "init:$x" };
our sub set-initial(&h) is export { &initial = &h }
my &call-initial = -> { initial(9) };
our sub run-initial() is export { call-initial() }
our sub initial-info() is export {
    (&initial.defined, &initial.arity, &initial(2), [1, 2].map(&initial).join(",")).join("|")
}
