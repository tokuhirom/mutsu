# Member 9 of 9 modules that load each other (t/modules/module-use-cycle-clique.t):
# each `use`s the later ones at compile time and `require`s the earlier ones in
# a routine, so the modules form a cycle for the parse-time scan but not at run time.

unit module UseCycle9;

our sub name() is export { 'UseCycle9' }
sub marker9() is export { 9 }
our sub load-earlier() is export {
    require UseCycle1;
    require UseCycle2;
    require UseCycle3;
    require UseCycle4;
    require UseCycle5;
    require UseCycle6;
    require UseCycle7;
    require UseCycle8;
    9
}
