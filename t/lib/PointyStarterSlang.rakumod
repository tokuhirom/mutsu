# Fixture for t/lang/parsing/slang-pointy-block-starter.t: a slang that adds
# lexemes to the grammar's `pointy-block-starter` token, as Slang::Lambda does.
use v6.e.PREVIEW;

our role Starter {
    token pointy-block-starter { '->' | '<->' | 'λ' | '↔' }
}

sub EXPORT() {
    $*LANG.refine_slang('MAIN', Starter, role { });
    BEGIN Map.new
}
