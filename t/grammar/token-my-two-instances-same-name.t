use Test;

plan 4;

# Two objects each declaring `my token FULLRE { ... }` under the same short
# name, inside the same class, must keep separate identities. mutsu's
# `token_defs` registry is keyed by `package::name` alone (with no notion of
# "this particular closure creation" vs "that one"), so a NON-multi
# declaration REPLACES whatever was registered under that key
# (`Registry::insert_token_def`). The second object's `method build` call
# re-declares `FULLRE` under the exact same registry key as the first
# object's declaration, silently overwriting it -- so a `&FULLRE` reference
# captured by the FIRST object, at match time, used to resolve to the
# SECOND object's captured regex instead of its own (issue #8680).
#
# Real Raku has no such problem: each `my token` declaration is a genuinely
# fresh, lexically-scoped code object per closure creation, not a
# global-registry entry keyed by name.
class TMTISN-Holder {
    has $.re;
    method build($str) {
        my $fullre = $str;
        my token FULLRE { ^ <{$fullre}> $ };
        $!re = &FULLRE;
    }
}

my $h3a = TMTISN-Holder.new;
$h3a.build('foo');
my $h3b = TMTISN-Holder.new;
$h3b.build('bar');

ok "foo" ~~ $h3a.re, 'the FIRST object captured &NAME still matches its own pattern after a second object re-declares the same token name';
nok "bar" ~~ $h3a.re, 'and does not match the SECOND object pattern';
ok "bar" ~~ $h3b.re, 'the SECOND object captured &NAME matches its own pattern';
nok "foo" ~~ $h3b.re, 'and does not match the FIRST object pattern';
