use Test;

# Pin for the (B) per-store env-write gate rw-redispatch slot-read fix
# (docs/lexical-scope-slot-campaign.md, "(B) per-store env-write gate — burndown").
#
# A nextsame/callsame (multi sub or multi method) or a proto `{*}` redispatch must
# forward the FIRST candidate's / proto body's CURRENT (body-mutated) rw parameter
# value to the next candidate. That candidate body runs compiled, so its `$x = ...`
# mutation lands in a VM local slot. Under MUTSU_GATE_LOCAL_ENV_WRITE the store's
# env mirror is skipped, so the redispatch used to read the STALE env value (the
# entry-time / decl-seed value) and forward it, breaking the rw chain:
#   - nextsame sub:    `$v` ended 1010 instead of 1011 (first `+1` not chained)
#   - proto method:    candidate saw the caller value 1 instead of the body's 30
# The fix reads the live value slot-first from the currently-executing frame
# (`self.current_code`/`self.locals`) — `first_candidate_rw_value` on the
# nextsame/callsame path, and passing the compiled proto-body `CompiledCode` to
# `proto_rw_redispatch_args` on the `{*}` path. Gate OFF (default) is byte-identical
# (env mirrors the slot). This file passes gate-OFF and would fail gate-ON before
# the fix.

plan 8;

# --- nextsame in a multi SUB chains the first candidate's rw write ---
{
    multi pr(Int $x is rw) { $x = $x + 1; nextsame }
    multi pr($x is rw)     { $x = $x + 1000 }
    my $v = 10;
    pr($v);
    is $v, 1011, 'nextsame sub chains the first candidate rw write';
}

# --- three-candidate nextsame chain in a multi SUB ---
{
    multi tri(Int $x is rw where * < 100)   { $x = $x + 1;   nextsame }
    multi tri(Int $x is rw where * < 10000) { $x = $x + 100; nextsame }
    multi tri($x is rw)                     { $x = $x + 1000 }
    my $v = 5;
    tri($v);
    is $v, 1106, 'three-candidate nextsame+rw chain in a sub';
}

# --- differently-named rw params still chain (source routes by position) ---
{
    multi nm(Int $a is rw) { $a = $a + 1; nextsame }
    multi nm($b is rw)     { $b = $b + 1000 }
    my $v = 10;
    nm($v);
    is $v, 1011, 'differently-named rw params chain in a sub';
}

# --- nextsame in a multi METHOD chains the rw write ---
{
    class C {
        multi method pr(Int $x is rw) { $x = $x + 1; nextsame }
        multi method pr($x is rw)     { $x = $x + 1000 }
    }
    my $v = 10;
    C.pr($v);
    is $v, 1011, 'nextsame method chains the first candidate rw write';
}

# --- proto METHOD body mutation is visible to the candidate ---
{
    class C1 {
        proto method m($x is rw) { $x = 30; {*} }
        multi method m(Int $x is rw) { $x = $x + 3 }
    }
    my $v = 1; C1.new.m($v);
    is $v, 33, 'proto method body mutation reaches the candidate';
}

# --- proto method aliases by position, not by name ---
{
    class C2 {
        proto method m($a is rw) { $a = 10; {*} }
        multi method m(Int $b is rw) { $b = $b * 5 }
    }
    my $v = 2; C2.new.m($v);
    is $v, 50, 'proto method redispatch aliases by position';
}

# --- proto method with no body mutation; candidate mutates from caller value ---
{
    class C3 {
        proto method m($x is rw) { {*} }
        multi method m(Int $x is rw) { $x = $x + 9 }
    }
    my $v = 4; C3.new.m($v);
    is $v, 13, 'proto method candidate rw write propagates without body mutation';
}

# --- `is raw` proto method param chains like `is rw` ---
{
    class C5 {
        proto method m($x is raw) { $x = 7; {*} }
        multi method m(Int $x is raw) { $x = $x + 1 }
    }
    my $v = 1; C5.new.m($v);
    is $v, 8, 'is raw proto method param chains like is rw';
}
