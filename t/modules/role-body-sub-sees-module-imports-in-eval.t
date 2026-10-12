use Test;
use lib 't/lib';

# A role body's `sub` belongs to the role's compunit, wherever the role is
# composed: it sees the routines imported into the role's module, even when
# the composing class is declared by an EVAL inside a sub, which leaves the
# sub's frame on the routine stack while the role body runs (#12895). Every
# expectation was checked against rakudo.

plan 4;

sub in-sub-eval { EVAL q{use RoleBodyImport::Role; class A does RoleBodyImport::Role { }; A.new.h} }
is in-sub-eval(), 42, 'a role-body constant calls a module-imported routine (EVAL in a sub)';

sub in-sub-eval-direct {
    EVAL q{use RoleBodyImport::Role; class B does RoleBodyImport::Role { }; B.new.direct}
}
is in-sub-eval-direct(), 42, 'a role-body sub called from a method sees the module import';

is EVAL(q{use RoleBodyImport::Role; class C does RoleBodyImport::Role { }; C.new.h}), 42,
    'the same from a file-scope EVAL';

sub in-sub {
    use RoleBodyImport::Role;
    my class D does RoleBodyImport::Role { }
    D.new.h
}
is in-sub(), 42, 'the same from a sub without EVAL';
