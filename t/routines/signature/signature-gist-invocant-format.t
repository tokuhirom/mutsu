use v6;
use Test;

# ADR-0019 E9-pre found that Signature.gist rendered the method invocant as
# `(C:, ...)` instead of raku's `(C $:: ...)` -- an anonymous typed invocant
# renders as `Type $::`, an explicitly named one keeps its name (`$self::`),
# and the separator after the invocant marker is a space, not a comma.
# Verified against Rakudo v2026.06.

class C {
    method m1($self: Int $x) { }
    method m2(C $s: Int $x) { }
    method m3() { }
    method m4(Int $x, Str $y) { }
    method m5($: Int $x) { }
    multi method m(Int $x) { }
    multi method m(Str $x) { }
}

is C.^lookup('m1').signature.gist, '($self:: Int $x, *%_)', 'explicit named invocant';
is C.^lookup('m2').signature.gist, '(C $s:: Int $x, *%_)', 'explicit typed+named invocant';
is C.^lookup('m3').signature.gist, '(C $:: *%_)', 'implicit invocant, no params';
is C.^lookup('m4').signature.gist, '(C $:: Int $x, Str $y, *%_)', 'implicit invocant, two params';
is C.^lookup('m5').signature.gist, '($:: Int $x, *%_)', 'explicit anonymous untyped invocant';
is C.^lookup('m').candidates.map(*.signature.gist).join(' | '),
   '(C $:: Int $x, *%_) | (C $:: Str $x, *%_)', 'multi candidates';

# The same invocant rendering feeds X::Multi::NoMatch's candidate listing.
my $err = try {
    class WorkingTie {
        multi method has-tie(Int $z) { }
        multi method has-tie(Str $z) { }
    }
    WorkingTie.new.has-tie([1, 2, 3]);
};
like $!.message, /'(WorkingTie $:: Int $z'/, 'NoMatch candidate list uses the same invocant format';

done-testing;
