use v6;
use Test;

# `format_method_candidate_signatures`'s guard that skips the implicit `*%_`
# slurpy from every candidate signature's per-param rendering was gated on
# `pd.named`, but the implicit slurpy carries `named: false` (see
# method_signature_shared.rs's `implicit_method_named_slurpy_param`), so the
# guard never fired and each candidate showed a duplicate `Any *%_, *%_`
# tail. Also, `format_call_arg_profile`'s positional-argument type never got
# its `:D`/`:U` definedness smiley, unlike the invocant. Verified against
# Rakudo v2026.06.

class WorkingTie {
    multi method has-tie(Int $z) { }
    multi method has-tie(Str $z) { }
}

my $err = try {
    WorkingTie.new.has-tie([1, 2, 3]);
};
is $!.message,
   "Cannot resolve caller has-tie(WorkingTie:D: Array:D); none of these signatures matches:\n"
   ~ "    (WorkingTie \$:: Int \$z, *\%_)\n"
   ~ "    (WorkingTie \$:: Str \$z, *\%_)",
   'candidate signatures show a single *%_ tail, and the arg profile carries its :D smiley';

class Bar {
    multi method baz(Int $z) { }
}
my $err2 = try {
    Bar.new.baz(Array);
};
like $!.message, /'baz(Bar:D: Array:U)'/, 'a bare type-object argument gets the :U smiley';

done-testing;
