use v6;
use Test;

# A module with a `sub EXPORT` that builds its own Map does not hand its
# importer the operators it imported for itself: the importer's parse knows
# only the core operators, so a module-only operator is a compile-time error
# there and `eq` stays the core operator. A hook that returns another module's
# export stash does pass those operators on. mutsu approximated every hook as
# re-exporting all of its module's imports, so the importer parsed `eq` as a
# call of a candidate it could not see (#18496).

plan 4;

sub run-code(Str $code) {
    my $proc = run $*EXECUTABLE, '-It/lib', '-e', $code, :out, :err;
    my $out = $proc.out.slurp(:close).trim;
    my $err = $proc.err.slurp(:close);
    %(:$out, :$err)
}

my %own = run-code(q[use JunctionOpExportSub; say 1 ± 2]);
is %own<out>, '', 'a module-only operator is not callable in the importer';
like %own<err>, /'SORRY'/, 'it is rejected when the importer compiles';

is run-code(q[use JunctionOpExportSub; print ('yes' eq any(<y yes>)).raku])<out>,
    'any(Bool::False, Bool::True)', 'eq is the core operator in the importer';

is run-code(q[use JunctionOpStashRelay; print 1 ± 2])<out>, 'plus-minus',
    'a hook returning another module\'s export stash passes its operators on';
