# Member 6 of 9 modules that load each other (t/modules/module-use-cycle-clique.t):
# each `use`s the later ones at compile time and `require`s the earlier ones in
# a routine, so the modules form a cycle for the parse-time scan but not at run time.
use UseCycle7;
use UseCycle8;
use UseCycle9;

unit module UseCycle6;

our sub name() is export { 'UseCycle6' }
sub marker6() is export { 6 }
our sub load-earlier() is export {
    require UseCycle1;
    require UseCycle2;
    require UseCycle3;
    require UseCycle4;
    require UseCycle5;
    6
}
