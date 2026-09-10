use Test;

plan 9;

# Writing $_ inside a `given` writes back to its own source var.
{
    my $v = 1;
    given $v { $_ = 5 }
    is $v, 5, 'given $_ writeback to source var works';
}

# --- Collection .gist honors per-element custom .gist ---
{
    my class Inst { method gist { 'INST' } }
    is (Inst.new, 1, 2).gist, '(INST 1 2)', 'list element instance custom gist';
    is [Inst.new, 5].gist, '[INST 5]', 'array element instance custom gist (brackets)';
    is %(a => Inst.new).gist, '{a => INST}', 'hash value instance custom gist';
    is (1, [2, Inst.new], (3,)).gist, '(1 [2 INST] (3))',
        'nested collections keep bracket style with custom gist';
}

# Type objects with a user-defined gist render via that gist.
{
    my class T { method gist { 'TG' } }
    is T.gist, 'TG', 'type object honors user gist';
    is (T, 1).gist, '(TG 1)', 'type object custom gist inside a list';
}

# A plain type object without custom gist still renders (Name).
is Int.gist, '(Int)', 'plain type object gist unchanged';

# --- Strict ASCII decode throws on bytes > 127 ---
{
    my $buf = "fo\x[E9]o".encode('latin-1');
    dies-ok { $buf.decode('ascii') }, 'ASCII decode dies on byte > 127';
}
