# A module whose last statement is a package block with a qualified name: the
# unit's tail value is that package's type object (#12086), and it must not be
# looked up by name while the module is still loading.
sub private-helper { 'helper' }

package PackageTailQualified::Inner {
    our $answer = 42;
    our sub hello { private-helper() ~ '!' }
}
