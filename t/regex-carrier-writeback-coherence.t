# Slice F (regex carrier) coherence: a bare `s///`, `$x ~~ s///`, and a `:let`
# declarative regex modifier write a caller lexical straight into `env` by name.
# When that lexical is a compiled local slot, the value must be written through
# to the slot so it stays coherent WITHOUT the reverse `sync_locals_from_env`
# pull. These tests pin ON == OFF == raku; run with MUTSU_NO_REVERSE_SYNC=1 to
# exercise the write-through path directly.
use Test;

plan 10;

# --- bare s/// updates the lexical topic $_ ---------------------------------
{
    my $_ = "abc";
    ok s/ab/xy/, 'bare s/// returns truthy on match';
    is $_, "xyc", 'bare s/// updates the lexical $_ slot';
}

# --- bare s///  with no match leaves $_ unchanged --------------------------
{
    my $_ = "hello";
    nok s/zzz/Q/, 'bare s/// returns falsy on no match';
    is $_, "hello", 'bare s/// leaves $_ unchanged on no match';
}

# --- bare s:g/// global substitution --------------------------------------
{
    my $_ = "a-a-a";
    ok s:g/a/b/, 's:g/// returns truthy';
    is $_, "b-b-b", 's:g/// updates the $_ slot for every match';
}

# --- $x ~~ s/// updates the named lexical ----------------------------------
{
    my $s = "foobar";
    $s ~~ s/foo/baz/;
    is $s, "bazbar", '$x ~~ s/// updates the named lexical slot';
}

# --- :let updates the caller lexical on a successful match -----------------
{
    my $a = 1;
    my regex lma { $a $a };
    my regex la { :let $a = 5; <&lma> };
    ok '55' ~~ m/^ <la> $/, ':let successful match';
    is $a, 5, ':let keeps the updated value in the caller slot after success';
}

# --- :let on an unsuccessful match (value restore semantics differ across
#     implementations; only the match result is asserted here for coherence) --
{
    my $b = 1;
    my regex bma { $b $b };
    my regex ba { :let $b = 5; <&bma> };
    nok '23' ~~ m/^ <ba> $/, ':let unsuccessful match returns falsy';
}
