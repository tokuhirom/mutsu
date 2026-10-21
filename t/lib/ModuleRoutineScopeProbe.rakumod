# A loaded module's routines, probing by name what they can see (ADR-12529):
# their own file scope and package, never the importing program's lexicals or
# the names the importer's own top-level `use`s imported.
unit module ModuleRoutineScopeProbe;

class OwnClass { method hi { 'own' } }

sub probe-eval-var is export { (try EVAL '$importer-var') // 'not visible' }
sub probe-indirect-var is export { (try ::('$importer-var')) // 'not visible' }
sub probe-eval-type is export { (try EVAL 'ImportedType.hi') // 'not visible' }
sub probe-indirect-type is export { (try ::('ImportedType').hi) // 'not visible' }
sub probe-eval-constant is export { (try EVAL 'IMPORTED-K') // 'not visible' }
sub probe-own-class-eval is export { EVAL 'OwnClass.hi' }
sub probe-own-class-indirect is export { ::('OwnClass').hi }
sub probe-own-package is export { EVAL 'ModuleRoutineScopeProbe.^name' }
sub probe-own-typed-eval is export {
    EVAL 'class EvalDeclared { has $.v }; my EvalDeclared $x .= new(:v(7)); $x.v'
}
