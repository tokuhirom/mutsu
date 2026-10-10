use Test;

# An unbound `&*name` is not a callable: `so &*NOPE` is False and `//` takes
# its default. Found via Red's `create-comment-to-caller`
# (`&*RED-COMMENT-SQL ?? ... !! ...`), distribution RedX::HashedPassword.

plan 5;

ok !(so &*NOPE), 'unbound &*name is falsy';
is (&*NOPE // 'dflt'), 'dflt', 'unbound &*name is undefined for //';
sub probe { so ($*A or &*B) }
my $*A = False;
ok !probe(), '&*B in a routine is falsy when never bound';
{
    my &*B = { 1 };
    ok probe(), 'a bound &*B is truthy';
}
ok &*chdir.defined, 'core dynamic routine &*chdir stays callable';
