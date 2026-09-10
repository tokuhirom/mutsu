use MONKEY-SEE-NO-EVAL;
use Test;

# EVAL's caller-lexical visibility has two independent directions: WRITES from
# an EVAL'd string into the caller's lexicals already worked (the carrier
# writeback, see t/eval-carrier-precise-writeback.t and friends), but READS —
# the EVAL'd string seeing the caller's *current* value of an already-declared
# lexical — silently returned a stale placeholder in one specific, easy-to-hit
# shape: an `EVAL '...'` used as a bare STATEMENT (its own return value
# discarded), anywhere other than the tail/last position of its enclosing
# chunk.
#
# Root cause: EVAL resolves the caller's lexicals by NAME against
# `Interpreter::env` (it has no compile-time knowledge of the caller's local
# slots — the EVAL'd string is a fresh, separately-compiled unit). A plain
# lexical's *slot* only mirrors into `env` on every write when the
# process-global, monotonic `REFLECTIVE_NAME_ACCESS_SEEN` flag has latched
# (see `crate::opcode::reflective_name_access_possible`); otherwise a
# perf-motivated gate (`vm_var_assign_set_local.rs`'s `skip_env_write`) keeps
# a slot-only local out of `env` entirely. The flag latches during
# `scan_reflective_name_access` (`opcode.rs`), which used to recognize only
# the tail/expression call shapes `CallFunc`/`CallFuncNamed` -- but a bare
# `EVAL '...';` statement (its value never used) compiles to `ExecCall` (or,
# with named args like `:lang`, `ExecCallPairs`), which that scan never
# matched. So a program whose only `EVAL` calls are bare statements never set
# the flag at all: every plain lexical it might read stayed a stale
# placeholder in `env`.
#
# EVERY EVAL call in this file is deliberately a bare statement (never
# assigned, never a call argument, never a block's tail expression) so this
# file pins exactly that gap -- an `is $result, ...` after an `EVAL
# q[$result = ...];` on its own line, never `is EVAL(...), ...`.
#
# For the same reason, every "sub" this file needs is an ANONYMOUS sub bound
# to a lexical (`my &f = sub (...) { ... }`), never a NAMED `sub f(...) {...}`
# declaration: a *named* sub is compiled lazily and is invisible to its
# enclosing frame's free-variable analysis, so `compute_needs_env_sync`
# conservatively keeps EVERY local of any frame that declares one
# permanently mirrored into `env` (see `defines_lazy_body` in `opcode.rs`) --
# regardless of whether that frame's EVAL calls are reflective. A named sub
# anywhere in this file would silently make every subtest pass whether or not
# the EVAL-read fix is present, defeating the point of a regression test.
#
# Not covered here (a separate, narrower, pre-existing gap, unrelated to
# caller-lexical *visibility*): reading a `my` declared textually AFTER the
# EVAL call in the same block. Both raku and mutsu treat this differently
# from a normal forward reference; see
# todo/tickets/eval-forward-declared-lexical-read.md.

plan 16;

# --- 1. The ticket's own repro: read, then write, then read again, with a
# trailing plain statement so the read EVAL is provably mid-chunk (ExecCall),
# never tail. ---
{
    my $x = 5;
    my $before;
    EVAL q[$before = $x];
    EVAL q[$x = 7];
    is $before, 5, 'EVAL sees the current value of a caller lexical on first read';
    is $x, 7, 'and a later EVAL write still lands (already worked pre-fix)';
}

# --- 2. Read-only, no subsequent write anywhere for this variable. ---
{
    my $y = 11;
    my $seen;
    EVAL q[$seen = $y];
    my $unrelated = 1 + 1; # keeps the EVAL out of tail position, no-op otherwise
    is $seen, 11, 'a read-only lexical is visible to a mid-chunk EVAL';
}

# --- 3. Several frames up: nested anonymous subs, EVAL as a bare statement
# deep inside. The read result comes back through an ordinary `return` (a
# well-tested, unrelated mechanism), not by writing into a distant ancestor
# frame's own lexical from inside the EVAL -- propagating an EVAL's reflective
# write back through more than one intervening call frame is a separate,
# narrower gap this ticket does not cover (only the frame that directly calls
# EVAL gets its own locals reconciled; see `writeback_carrier_writes`). ---
{
    my $z = 42;
    my &outer-fn = sub {
        my &inner-fn = sub {
            my $local-result;
            EVAL q[$local-result = $z];
            return $local-result;
        };
        return inner-fn();
    };
    is outer-fn(), 42, 'EVAL reads a lexical from several enclosing routine frames up';
}

# --- 4. `our` / package vars: read then write, both as bare statements. ---
{
    our $pkg-var = 3;
    my $read-back;
    EVAL q[$read-back = $pkg-var];
    is $read-back, 3, 'EVAL reads an `our` package var';
    EVAL q[$pkg-var = 4];
    is $pkg-var, 4, 'and can still write it';
}

# --- 5. `state` vars: each call's current value is visible. ---
{
    my @seen;
    my &with-state = sub {
        state $s = 0;
        $s++;
        EVAL q[@seen.push: $s];
    };
    with-state();
    with-state();
    is-deeply @seen, [1, 2], 'EVAL reads the current-call value of a state var';
}

# --- 6. Topic `$_` inside a `for` loop body. ---
{
    my @seen;
    for 1, 2, 3 {
        EVAL q[@seen.push: $_];
    }
    is-deeply @seen, [1, 2, 3], 'EVAL reads the topic $_ on each loop iteration';
}

# --- 7. Sub parameters. Read result comes back via `return`, for the same
# reason as case 3 above (the write side, several frames up, is out of
# scope here). ---
{
    my &with-param = sub ($a) {
        my $seen;
        EVAL q[$seen = $a];
        return $seen;
    };
    is with-param(99), 99, 'EVAL reads a sub parameter';
}

# --- 8. Sigilless / @ / % / & sigils. ---
{
    my @arr = 1, 2, 3;
    my $seen-arr;
    EVAL q[$seen-arr = @arr.elems];
    is $seen-arr, 3, 'EVAL reads an @ array lexical';

    my %h = a => 1, b => 2;
    my $seen-hash;
    EVAL q[$seen-hash = %h.elems];
    is $seen-hash, 2, 'EVAL reads a % hash lexical';

    my &c = sub { 123 };
    my $seen-code;
    EVAL q[$seen-code = c()];
    is $seen-code, 123, 'EVAL reads a lexical & code var';
}

# --- 9. A closure MADE inside the EVAL'd string, capturing a caller lexical,
# called after the EVAL returns -- and after a further OUTSIDE mutation, to
# confirm it shares the live variable rather than snapshotting a stale copy. ---
{
    my $w = 1;
    my $cl;
    EVAL q[$cl = { $w }];
    $w = 77;
    is $cl(), 77, 'a closure made inside EVAL tracks a later mutation of the caller lexical';
}

# --- 10. `EVAL :lang<Raku>` (a named-arg call site, ExecCallPairs) is just as
# reflective as the plain form. ---
{
    my $v = 55;
    my $seen;
    EVAL(q[$seen = $v], :lang<Raku>);
    is $seen, 55, 'EVAL with :lang reads the caller lexical too';
}

# --- 11. Nested EVAL: the inner EVAL'd string still reads the outermost
# caller's lexical. ---
{
    my $n = 8;
    my $seen;
    EVAL q[EVAL q[$seen = $n]];
    is $seen, 8, 'a nested EVAL still reads the outermost caller lexical';
}

# --- 12. Negative case: a `my` declared INSIDE the EVAL'd string must not
# leak out and overwrite a same-named caller lexical -- it only shadows it
# for the duration of the EVAL'd snippet. (A *new* name the EVAL declares
# cannot be probed at all without a static "undeclared variable" compile
# error, since raku resolves that check against the OUTER script's compile-time
# scope, which never saw the EVAL's internal declaration -- so shadowing an
# existing caller lexical is the only way to observe this that both raku and
# mutsu can even parse.)
{
    my $leaked = 111;
    EVAL q[my $leaked = 999];
    is $leaked, 111,
        'a my declared inside EVAL shadows, not overwrites, a same-named caller lexical';
}
