use TransitiveLeakInner;
unit module TransitiveLeakOuter;

class OuterClass is export { method who() { 'outer-class' } }

# The module's own routines must keep resolving the short names it imported,
# even though the importer of this module must not see them.
sub outer-uses-inner() is export { InnerClass.new.who ~ '/' ~ InnerConst }
class OuterHolder is export {
    method make-inner() { InnerClass.new.who }
}

# This module's own non-exported `constant`s and enum values -- see the note in
# TransitiveLeakInner.rakumod (#7787).
constant OUTER-PRIVATE = 'outer-private';
enum OuterEnum <OUTER-GAMMA OUTER-DELTA>;

sub outer-reads-private() is export { OUTER-PRIVATE ~ '/' ~ OUTER-GAMMA.key }
class OuterReader is export {
    method peek() { OUTER-PRIVATE ~ '/' ~ OUTER-DELTA.key }
}

# What this module can see of a module it `use`d: rakudo does not make an
# unexported constant of TransitiveLeakInner visible here either.
sub outer-peeks-inner-private() is export {
    my $v = ::('INNER-PRIVATE');
    ($v.defined and $v !~~ Failure) ?? $v.gist !! 'MISSING';
}

# The inner module's own routines/methods must keep reading its unexported
# constants and enum values; the importer of *this* module cannot reach them
# directly (they are exported one hop away), so probe them from here.
sub outer-probes-inner-reads() is export { inner-reads-private() }
sub outer-probes-inner-method() is export { InnerReader.new.peek() }
sub outer-probes-inner-const() is export { InnerConst }
