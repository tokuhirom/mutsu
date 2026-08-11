# Interface to activate slangs at compile time inside slang modules

sub EXPORT(
  Mu $grammar  is copy,
  Mu $actions? is copy,
  Mu $legacy-grammar?,
  Mu $legacy-actions?
) {

    # The EXPORT sub that actually does the slanging
    my sub EXPORT() {
        my $LANG := $*LANG;

        unless $LANG.^name.starts-with('Raku::') {
            $grammar := $legacy-grammar unless $legacy-grammar<> =:= Mu;
            $actions := $legacy-actions unless $legacy-actions<> =:= Mu;
        }

        $LANG.define_slang('MAIN',
          $grammar<> =:= Mu
            ?? $LANG.slang_grammar('MAIN')
            !! $LANG.slang_grammar('MAIN').^mixin($grammar<>),
          $actions<> =:= Mu
            ?? $LANG.slang_actions('MAIN')
            !! $LANG.slang_actions('MAIN').^mixin($actions<>)
        );

        BEGIN Map.new
    }

    multi sub postcircumfix:<{ }>(Mu $/, Str:D $key) {
        $/.hash.AT-KEY($key)
    }
    multi sub prefix:<~>(Mu $/) {
        $/.Str
    }

    # Export the EXPORT sub that does the slanging
    Map.new: (
      '&EXPORT'              => &EXPORT,
      '&prefix:<~>'          => &prefix:<~>,
      '&postcircumfix:<{ }>' => &postcircumfix:<{ }>  # UNCOVERABLE
    )
}

# vim: expandtab shiftwidth=4
