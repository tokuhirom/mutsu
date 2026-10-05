unit module ToplevelMarkersUnit;

our int32 constant VERSION = 3;
constant LABEL = 'unit-label';
constant EVAL-ONLY = 'unit-eval';
constant $pat = 'abc';
my Int $counter = 0;

# Regex `$pat` interpolation reads the `constant` marker; the typed `my`
# keeps its type check for the module's own routines.
our sub matches($s) { so $s ~~ /$pat/ }
our sub bump() { $counter += 1; $counter }
our sub bump-bad() { $counter = 'nope'; $counter }
our sub version() { VERSION }
our sub label() { LABEL }
our sub eval-term() { use MONKEY-SEE-NO-EVAL; EVAL 'EVAL-ONLY' }
our sub eval-label() { use MONKEY-SEE-NO-EVAL; EVAL 'LABEL' }
our sub eval-constant-local() {
    my constant LABEL = 'local-constant';
    use MONKEY-SEE-NO-EVAL;
    EVAL 'LABEL'
}
our sub eval-label-local() {
    my \LABEL = 'local-label';
    use MONKEY-SEE-NO-EVAL;
    EVAL 'LABEL'
}
