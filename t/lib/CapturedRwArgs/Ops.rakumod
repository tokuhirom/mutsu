# Fixture for t/routines/dispatch/captured-multi-dispatcher-rw-argument.t:
# rw/readonly candidate pairs in the shape of Red::Operators' `infix:<lt>`. The
# left operand's `Str` is a user method, so ranking the `Str() $a` candidates
# runs user code.
unit module CapturedRwArgs::Ops;
class Tag is export { method Str { my $x = 1; 'tag' } }
multi cap-lt(Tag $a, DateTime $b is rw) is export { 'dt-rw' }
multi cap-lt(Tag $a, DateTime $b is readonly) is export { 'dt-ro' }
multi cap-lt(Tag $a, Tag $b) is export { 'tags' }
multi cap-lt(Tag $a, Str() $b is rw) is export { "rw:$b" }
multi cap-lt(Tag $a, Str() $b is readonly) is export { "ro:$b" }
multi cap-lt(Str() $a is rw, Tag $b) is export { "rw-l:$a" }
multi cap-lt(Str() $a is readonly, Tag $b) is export { "ro-l:$a" }
