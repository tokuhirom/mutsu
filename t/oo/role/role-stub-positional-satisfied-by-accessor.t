use Test;

# rakudo satisfies a role's required NON-MULTI method by NAME -- the stub's
# signature is advisory, not enforced. A public attribute's accessor is a
# concrete method like any other, so it satisfies a stub even when the stub
# declares positional parameters the accessor does not take (#9758). A
# stubbed *multi* is different: rakudo keeps per-candidate signature
# enforcement there, so an accessor only satisfies a multi stub when it is
# nullary.

plan 3;

role K { method r($x) { ... } }
class E does K { has $.r = 1 }
is E.new.r, 1, 'an accessor satisfies a non-multi stub with a positional parameter';

role T { method r(::?ROLE:D: Int $x --> Str) { ... } }
class F does T { has $.r = 1 }
is F.new.r, 1, 'an accessor satisfies a fully typed non-multi stub with a positional parameter';

role M { multi method r(Int $x) { ... } }
dies-ok { EVAL 'class G does M { has $.r = 1 }' },
    'an accessor does NOT satisfy a multi stub with a positional parameter';
