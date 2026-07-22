use Test;

# A `:U:`/`:D:` multi-method pair must not leak a spurious "ambiguous dispatch"
# state onto a LATER, unrelated method call.
#
# Root cause: an invocant-less method resolution (from an introspection /
# existence probe such as the `.Str` identity check) skips every invocant
# type/definedness constraint in `method_args_match_for_invocant`, so BOTH the
# `:U:` and `:D:` candidates "match" and tie — setting the internal
# `dispatch_ambiguous` flag even though the real, invocant-aware dispatch resolves
# cleanly. The flag then poisoned the NEXT real dispatch: a defined-instance
# `.gist`/`.Str` call (a U/D multi) left the flag set, so a following `self.pairs`
# was wrongly reported as "Ambiguous call to 'pairs(...)'".

plan 6;

role R does Associative does Iterable {
    method AT-KEY($)  { die 'stub AT-KEY' }
    method keys()     { die 'stub keys' }
    method pairs(::?ROLE:D:) { self.keys.map: { Pair.new($_, self.AT-KEY($_)) } }
    proto method gist(|) {*}
    multi method gist(::?ROLE:U:) { self.Mu::gist }
    multi method gist(::?ROLE:D:) { '{' ~ self.pairs.sort(*.key).map(*.gist).join(", ") ~ '}' }
    proto method Str(|) {*}
    multi method Str(::?ROLE:U:) { self.Mu::Str }
    multi method Str(::?ROLE:D:) { self.pairs.sort(*.key).join(" ") }
}
class C does R {
    has %!h;
    method AT-KEY($k) is raw { %!h.AT-KEY($k) }
    method keys()            { %!h.keys }
    submethod BUILD()        { %!h = (a => 1, b => 2) }
}

my $o = C.new;

# Both coercions call self.pairs; calling one must not poison the next.
is $o.gist, '{a => 1, b => 2}', '.gist (a :D: multi that calls self.pairs) works';
is $o.Str,  "a\t1 b\t2",          '.Str after .gist does not report pairs as ambiguous';
is $o.gist, '{a => 1, b => 2}', '.gist again still works';
is $o.Str,  "a\t1 b\t2",          '.Str again still works';

# The type-object variants still dispatch to the :U: multi.
is C.gist, '(C)', 'type-object .gist hits the :U: variant';
is C.Str,  '',    'type-object .Str hits the :U: variant';
