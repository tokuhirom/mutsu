use TopPkgNames::C;

package TopPkgNames::A {
    our sub a() { "a" ~ TopPkgNames::C::c() }
}
