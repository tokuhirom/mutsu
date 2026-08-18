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

# `format_method_candidate_signatures`'s per-param loop never branched on
# `pd.named`, so a named param rendered as a positional (`Any $x` instead of
# `:$x!`). See news/2026-08/candidate-signature-named-param-format.md.
class Foo {
    multi method bar(:$x!) { }
}
my $err3 = try {
    Foo.new.bar(y => [1, 2, 3]);
};
is $!.message,
   "Cannot resolve caller bar(Foo:D: :y(Array)); none of these signatures matches:\n"
   ~ "    (Foo \$:: :\$x!, *\%_)",
   'a required named param renders as :$x!, not a positional';

class C {
    multi method m(:$y) { }
    multi method m(Int $a) { }
}
my $err4 = try {
    C.new.m("hi", "there");
};
is $!.message,
   "Cannot resolve caller m(C:D: Str:D, Str:D); none of these signatures matches:\n"
   ~ "    (C \$:: :\$y, *\%_)\n"
   ~ "    (C \$:: Int \$a, *\%_)",
   'an optional named param renders as :$y, no trailing bang';

class D {
    multi method n(Int :$y!) { }
}
my $err5 = try {
    D.new.n(z => "hi");
};
is $!.message,
   "Cannot resolve caller n(D:D: :z(Str)); none of these signatures matches:\n"
   ~ "    (D \$:: Int :\$y!, *\%_)",
   'a typed required named param keeps its type: Int :$y!';

done-testing;
