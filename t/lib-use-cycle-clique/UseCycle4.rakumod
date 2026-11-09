# Member 4 of 9 modules that load each other (t/modules/module-use-cycle-clique.t):
# each `use`s the later ones at compile time and `require`s the earlier ones in
# a routine, so the modules form a cycle for the parse-time scan but not at run time.
use UseCycle5;
use UseCycle6;
use UseCycle7;
use UseCycle8;
use UseCycle9;

unit module UseCycle4;

our sub name() is export { 'UseCycle4' }
sub marker4() is export { 4 }
our sub load-earlier() is export {
    require UseCycle1;
    require UseCycle2;
    require UseCycle3;
    4
}
