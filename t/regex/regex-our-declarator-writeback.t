use Test;

# A regex-embedded `:our $var = ...;` declarator is a real package-scoped
# declaration (see raku-doc/doc/Language/regexes.rakudoc): the variable must
# be usable *within* the match (like `:my`) AND, after the match, its value
# must be visible through the package-qualified name (`$Pkg::var`) — the same
# way a plain, non-regex `our $var = ...;` writes through to package storage.
#
# Discovered via the doc-diff harness on raku-doc/doc/Language/regexes.rakudoc
# (around line 1612). Root cause: grammar `token`/`rule`/`regex` bodies
# compile their pattern into a structured `RegexAtom` tree ONE TIME (not
# re-parsed from a raw string per match), so `:our`/`:my`/etc. inside a
# grammar token is handled by `RegexAtom::VarDecl` in
# src/runtime/regex/regex_match_capture.rs — a completely separate code path
# from the raw-string declarative-prefix handling used by a plain `~~ / ... /`
# smartmatch (src/runtime/regex/regex_match_public.rs). The grammar-token path
# only ever evaluated the declarator's RHS expression and stashed it in a
# per-match capture dict for in-match lexical use, discarding `is_our`
# entirely -- so the package-scoped write-through never happened.
#
# A second, independent bug surfaced along the way: `Compiler::qualify_variable_name`
# unconditionally returned an UNQUALIFIED name whenever the compile-time
# `current_package` held a synthetic sub/method/closure state-scope pseudo-package
# (e.g. `Pkg::&foo/1`, assigned to every routine body purely for `state`-variable
# key uniqueness) -- which is every routine body, not just grammar tokens. That
# made a PLAIN (non-regex) `our $var = ...;` inside ANY named sub/method/closure
# silently qualify against no package at all instead of the real enclosing one
# (`Compiler::enclosing_package`), so `$Pkg::var` never saw a sub-local `our`
# declaration's value either. Fixed by having `qualify_variable_name` resolve
# against `enclosing_package` in that case, mirroring the existing
# `Compiler::runtime_current_package` fallback.

plan 8;

grammar HasOur {
    token TOP {
        :our $our = 'thor';
        $our \s+ is \s+ mighty
    }
}
ok HasOur.parse('thor is mighty'), 'grammar token with :our declarator matches';
is $HasOur::our, 'thor', ':our declarator inside a grammar token writes through to the package variable';

# A plain (non-grammar) token with a :our declarator, matched directly.
my token has-our-plain {
    :our $plain-our = 'plainvalue';
    foo
}
ok 'foo' ~~ &has-our-plain, 'plain (non-grammar) token with :our declarator matches';
is $GLOBAL::plain-our, 'plainvalue', ':our declarator inside a plain token writes through to GLOBAL';

# Regression check: a :my declarator inside a grammar token is unaffected --
# it stays regex/match-local and does NOT leak into package storage.
grammar HasMy {
    token TOP {
        :my $tmp = 'lexical';
        $tmp
    }
}
ok HasMy.parse('lexical'), 'grammar token with :my declarator still matches (regression check)';
nok defined($HasMy::tmp), ':my declarator inside a grammar token does NOT write through to the package';

# Regression check: a plain (non-regex) `our $var = ...;` inside a package sub
# writes through to package storage -- the general compiler-level fix.
package PlainOurPkg {
    sub set-it {
        our $x = 'hello';
    }
    set-it();
}
is $PlainOurPkg::x, 'hello', 'plain our $var = ...; inside a package sub writes through to package storage';

# The already-fixed :my/:constant caller-scope persistence (raw-string path,
# PR #6964) is unaffected by this change (different file/mechanism).
lives-ok {
    "aba" ~~ / (a) b :my $c = 1; /;
}, ':my declarator in a raw-string (non-grammar) regex still parses and runs';

done-testing;
