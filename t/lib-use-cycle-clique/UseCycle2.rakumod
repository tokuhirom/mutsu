# Member 2 of 9 modules that load each other (t/modules/module-use-cycle-clique.t):
# each `use`s the later ones at compile time and `require`s the earlier ones in
# a routine, so the modules form a cycle for the parse-time scan but not at run time.
use UseCycle3;
use UseCycle4;
use UseCycle5;
use UseCycle6;
use UseCycle7;
use UseCycle8;
use UseCycle9;

unit module UseCycle2;

our sub name() is export { 'UseCycle2' }
sub marker2() is export { 2 }
our sub load-earlier() is export {
    require UseCycle1;
    2
}
