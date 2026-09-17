use Test;

plan 4;

# A `my token`/`my rule` that interpolates a dynamic pattern (`<{$var}>`,
# `<$var>`) and is stored into an attribute for later use -- outliving the
# method call that declared it -- must keep its interpolated lexical's value,
# the same way a stored `/regex/` literal already does (see
# `regex_literal_closure_captures`). A `token`/`rule` declaration compiles
# through a completely different path (`token_decl_plans` /
# `CompiledTokenDeclPlan`, ADR-0019 F7) and did not share that fix: the
# declaration's embedded regex literal always carried `scope: None` (baked in
# at parse time), so by the time the token was actually matched -- well after
# the declaring frame had returned -- its interpolated lexical had already
# resolved to Nil, and matching threw "Null regex not allowed" instead of
# matching normally (issue #8662).
class TMCODF-Holder {
    has $.re;

    method build($str) {
        my $fullre = $str;
        my token FULLRE { ^ <{$fullre}> };
        $!re = &FULLRE;
    }
}

my $h = TMCODF-Holder.new;
$h.build('abc');
ok "abc" ~~ $h.re, 'a my token storing &NAME past its declaring frame still matches its interpolated <{...}> pattern';
nok "xyz" ~~ $h.re, 'and correctly fails to match a string the interpolated pattern does not describe';

# The `<$var>` interpolation spelling (dynamic sub-pattern lookup, distinct
# from `<{ code }>` closure interpolation) must close over its defining frame
# the same way.
class TMCODF-Holder2 {
    has $.re;

    method build($str) {
        my $fullre = $str;
        my token FULLRE { ^ <$fullre> };
        $!re = &FULLRE;
    }
}

my $h2 = TMCODF-Holder2.new;
$h2.build('abc');
ok "abc" ~~ $h2.re, 'a my token using <$var> interpolation also closes over its defining frame';
nok "xyz" ~~ $h2.re, 'and correctly fails to match a string the interpolated pattern does not describe';
