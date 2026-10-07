use v6;
use Test;

# The default rendering behind a user gist/Str/raku is a candidate of the
# frame (DeferralEntry::Native, ADR-11276 slice 4).
plan 5;

class A { method gist { "<" ~ callsame() ~ ">" } }
is A.new.gist, '<A.new>', 'callsame from gist reaches the default gist';

class C { method raku { "r:" ~ callsame() } }
is C.new.raku, 'r:C.new', 'callsame from raku reaches the default raku';

class D is A { method gist { "D" ~ nextsame() } }
is D.new.gist, '<D.new>', 'nextsame walks the user chain, then the default';

class E { method gist { "[" ~ callwith() ~ "]" } }
is E.new.gist, '[E.new]', 'callwith with no arguments';
is E.new.Str.starts-with('E'), True, 'an untouched Str is unaffected';
