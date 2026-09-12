# A miniature of the generated localization roles the Raku/L10N distributions
# ship (https://github.com/Raku/L10N) — `L10N::JA` 0.0.3 is the shape this
# mirrors, cut down to the handful of entries `t/lang/parsing/slang-l10n-vocabulary.t`
# exercises. The structure is deliberately verbatim rather than simplified: a
# role of `token <category>-<name> { <literal> }` declarations plus a `core2ast`
# method whose `%mapping` constant carries the identifier-position half, all
# registered by an EXPORT sub that mixes the role into the MAIN slang grammar.
#
# The spellings are ASCII where the point is the mechanism; `block-unless` is
# deliberately non-ASCII so the multi-byte identifier-boundary handling is
# pinned end to end and not only in the parser's own unit tests.

role L10N::Testish {
    use experimental :rakuast;
    token block-if { iffy }
    token block-else { elsey }
    token block-unless { なければ }
    token modifier-if { iffish }
    token scope-my { mine }
    token package-class { klass }
    token routine-sub { subby }
    token enum-True { yes }
    # An inert category: mutsu records word-infix translations but does not
    # consult them yet, and that must not make the whole vocabulary fail.
    token infix-eq { samesame }
    method core2ast {
        my constant %mapping = "yell", "say", "howmany", "elems";
        my $ast := self.ast;
        my $name := $ast ?? $ast.simple-identifier !! self.Str;
        if %mapping{$name} -> $original {
            RakuAST::Name.from-identifier($original)
        }
        else {
            $ast // RakuAST::Name.from-identifier($name)
        }
    }
}

my sub EXPORT($dontslang?) {
    unless $dontslang {
        my $LANG := $*LANG;
        $LANG.define_slang('MAIN',
          $LANG.slang_grammar('MAIN').^mixin(L10N::Testish)
        );
    }

    BEGIN Map.new
}
