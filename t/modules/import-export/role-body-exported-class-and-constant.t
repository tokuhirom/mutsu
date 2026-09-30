use lib 't/lib';
use Test;

# #9981: `my class ... is export` and `my constant ... is export` declared in a
# role body are compile-time declarations; they are importable without any class
# ever composing the role.
use RoleBodyExportedDecls;
use RoleBodyExportedDecls :extra;

plan 4;

is Foo.^name, 'RoleBodyExportedDecls::Foo', 'exported my class from a role body is imported';
is X, 5, 'exported my constant from a role body is imported';
is Tagged.^name, 'RoleBodyExportedDecls::Tagged', 'tagged exported class is imported with its tag';
ok Foo.new ~~ Foo, 'the imported class is usable';
