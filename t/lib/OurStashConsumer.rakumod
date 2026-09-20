use OurStashGen;

# A consumer compunit, to pin that a generated/imported CODE variable is
# reachable from routines DECLARED HERE and not only from this file's mainline.
# See t/modules/import-export/our-stash-generated-exports.t.

sub from-sub() is export { aa('sub') }
sub from-sub-amp() is export { &aa('amp') }

class Klass is export {
    method from-method() { aa('method') }
}

role Rolle {
    method from-role-method() { bb('role') }
}
sub make-rolle() is export { Rolle.new }

sub the-scalar() is export { $dd }
