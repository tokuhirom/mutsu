# Fixture for t/lang/parsing/slang-numish-kinds.t (ADR-15762, #15804): a
# `numish` action override that tells which capture the grammar gave the
# literal (`integer`, `decimal-number`, `radix-number`, or none), as
# overload::constant does, and answers "<kind>:<text>".
use v6.e.PREVIEW;
use nqp;

my sub atkeyish(Mu \h, \k) {
    nqp::atkey(nqp::findmethod(h, 'hash')(h), k)
}

our role Numish {
    method numish(Mu $/) {
        my $kind = atkeyish($/, 'integer') ?? 'integer'
            !! (atkeyish($/, 'decimal-number') || atkeyish($/, 'dec_number')) ?? 'decimal'
            !! (atkeyish($/, 'radix-number') || atkeyish($/, 'rad_number')) ?? 'radix'
            !! 'other';
        my $new = RakuAST::StrLiteral.new("$kind:{$/.Str}");
        self.attach: $/, $new;
        $new
    }
}

sub EXPORT() {
    $*LANG.refine_slang('MAIN', role { }, Numish);
    BEGIN Map.new
}
