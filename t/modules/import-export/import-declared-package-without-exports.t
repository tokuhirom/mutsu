use Test;

plan 4;

# `import Foo` of a package this unit declares itself, which exports nothing,
# is a no-op; the package's own routines stay reachable by qualified name.
module Plain { our sub hello() { 'hi' } }
lives-ok { EVAL 'import Plain; 1' }, 'import of a module without exports lives';
is Plain::hello, 'hi', 'qualified call still works';

class Thing { method m { 'm' } }
lives-ok { EVAL 'import Thing; 1' }, 'import of a class without exports lives';

# an unknown name is still an error
dies-ok { EVAL 'import NoSuchModuleAnywhere; 1' }, 'import of an unknown module dies';
