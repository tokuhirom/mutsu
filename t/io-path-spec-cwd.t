use Test;

plan 13;

# $*SPEC is the platform IO::Spec *type object* (not an instance), and is
# identical to IO::Path's default .SPEC.
nok $*SPEC.DEFINITE, '$*SPEC is a type object';
ok $*SPEC === IO::Path.new('.').SPEC, '$*SPEC === default IO::Path.SPEC';
is-deeply '.'.IO.SPEC, $*SPEC, "'.'.IO.SPEC defaults to \$*SPEC";

# IO::Spec is an inheritable built-in class.
my class MySpec is IO::Spec {}
is IO::Path.new('.', :SPEC(MySpec)).SPEC.^name, 'MySpec',
    '.new accepts a custom :SPEC subclass';

# IO::Path.new defaults .SPEC to a temp-overridden $*SPEC.
{
    my class TempSpec is IO::Spec {}
    temp $*SPEC = TempSpec;
    ok IO::Path.new('.').SPEC === TempSpec, '.new honors temp $*SPEC';
    ok '.'.IO.SPEC === TempSpec, ".IO honors temp \$*SPEC";
}

# `.IO` takes only named adverbs and IGNORES :CWD (unlike `.new`).
my $cwd = $*CWD.Str;
is '.'.IO(:CWD<<somewhere>>).CWD, $cwd, '.IO ignores :CWD param';

# `.new` DOES respect :CWD.
is IO::Path.new('.', :CWD</tmp/x>).CWD, '/tmp/x', '.new respects :CWD';

# .SPEC and .CWD are read-only: assignment throws X::Assignment::RO.
throws-like { '.'.IO.SPEC = my class :: is IO::Spec {} }, X::Assignment::RO,
    'cannot assign to .SPEC';
throws-like { '.'.IO.CWD = '/tmp/y' }, X::Assignment::RO,
    'cannot assign to .CWD';

# chdir into an IO::Path subclass instance respects its :CWD attribute.
my $sub = IO::Path::Unix.new('.', :CWD($cwd));
lives-ok { temp $*CWD; &*chdir($sub) }, 'chdir into IO::Path::Unix with :CWD lives';

# Basic IO::Path still works.
ok '/foo/bar'.IO ~~ IO::Path, 'Str.IO produces an IO::Path';
is '/foo/bar'.IO.basename, 'bar', '.basename works';
