use Test;
use lib $*PROGRAM.parent(2).add('lib');
use StashTermExport;

plan 6;

# A package term stored through its stash from another frame (here `sub
# EXPORT`) stays readable by its qualified name after that frame is gone.
is StashTerm::Yes.key, 'Yes', 'qualified name reads the stored value';
ok StashTerm::Yes.DEFINITE, 'it is the instance, not a package';
ok StashTerm::Yes === Yes, 'the same object the EXPORT map exported';
is StashTerm::<Maybe>.key, 'Maybe', 'the stash subscript agrees';
is (StashTerm::Maybe // 'fallback'), 'fallback', '// consults the user .defined';

sub install { class Local { has $.k }; Local::{'One'} = Local.new(k => 1) }
install();
is Local::<One>.k, 1, 'a stash term assigned in a sub of the same file';
