# Fixture for t/lang/parsing/slang-overload-constant-like.t (#15804): the
# mechanisms overload::constant uses -- a parametric `Numish[%handlers]` actions
# role that reads the Match by key (`nqp::atkey`), falls through with `nextsame`,
# and hands the handler's result back as a `RakuAST::Constant`.
use nqp;

my sub atkeyish(Mu \h, \k) {
    nqp::atkey(nqp::findmethod(h, 'hash')(h), k)
}

my role Numish[%handlers] {
    method numish(Mu $/) {
        my $handler;
        my $source;

        if atkeyish($/, 'integer') -> $v {
            $handler := %handlers<integer>;
            $source  := $v.Str;
        }
        elsif (atkeyish($/, 'decimal-number') || atkeyish($/, 'dec_number')) -> $v {
            $handler := %handlers<decimal>;
            $source  := $v.Str;
        }
        else {
            $handler := %handlers<numish>;
            $source  := $/.Str;
        }

        nextsame unless $handler;
        self.attach: $/, ::("RakuAST::Constant").new(nqp::decont($handler(nqp::p6box_s($source))))
    }
}

sub EXPORT(*@handlers) {
    my %handlers = @handlers.map({$_.name => $_});
    $*LANG.refine_slang('MAIN', role {}, Numish[%handlers]);
    BEGIN Map.new
}
