use v6;
use Test;
# TEMPORARY #9930 CI diagnostic -- remove.
unless %*ENV<DIAG9930> {
    %*ENV<DIAG9930> = '1';
    my @variants = (
        'base' => {},
        'jit-off' => { MUTSU_JIT => 'off' },
        'precomp-0' => { MUTSU_PRECOMP => '0' },
        'bytecode-0' => { MUTSU_PRECOMP_BYTECODE => '0' },
        'gc-off' => { MUTSU_GC => 'off' },
        'gc-on' => { MUTSU_GC => 'on' },
        'trir-0' => { MUTSU_TRIR => '0' },
        'memo-0' => { MUTSU_PARSE_MEMO => '0' },
        'fold-0' => { MUTSU_CONST_FOLD => '0' },
    );
    my @res;
    my $first-err = '';
    for @variants -> $v {
        my %saved;
        for $v.value.kv -> $k, $val { %saved{$k} = %*ENV{$k}; %*ENV{$k} = $val }
        my $p = run $*EXECUTABLE, $?FILE, :out, :err;
        my $e = $p.err.slurp(:close);
        $p.out.slurp(:close);
        for $v.value.keys -> $k { %*ENV{$k}:delete }
        @res.push("{$v.key}={$p.exitcode}");
        $first-err ||= $e.subst("\n", ' | ', :g) if $p.exitcode != 0;
    }
    if @res.grep(* !~~ /'=0'$/) {
        say "Bail out! variants: {@res.join(' ')} err1=[$first-err]";
    }
    else { say "1..1"; say "ok 1 - all variants pass: {@res.join(' ')}" }
    exit 0;
}

# Array-contextualizing a Seq held in a scalar (`@$s`) reifies it into a
# re-iterable list, matching Rakudo. A second `for @$s` / read must NOT throw
# X::Seq::Consumed. Regression for a bug surfaced by `zef list`
# (Zef::Pluggable!list-plugins iterates `@$backend` where a backend element is a
# Seq, across two calls).

plan 7;

# 1. Two `for @$s` loops both iterate the full Seq.
{
    my $s = (1, 2, 3).map({ $_ });
    my @first;
    my @second;
    for @$s -> $x { @first.push($x) }
    for @$s -> $x { @second.push($x) }
    is-deeply @first, [1, 2, 3], 'first  for @$s iterates all elements';
    is-deeply @second, [1, 2, 3], 'second for @$s re-iterates (no X::Seq::Consumed)';
}

# 2. A list-context read (.elems) followed by a `for @$s` still iterates.
{
    my $s = (10, 20, 30).map({ $_ });
    is @$s.elems, 3, '@$s.elems reifies without consuming';
    my @vals;
    for @$s -> $x { @vals.push($x) }
    is-deeply @vals, [10, 20, 30], 'for @$s after .elems still iterates all';
}

# 3. Repeated list-context aggregate reads see the same data.
{
    my $s = (1, 2, 3, 4).grep({ $_ %% 2 });   # Seq of (2, 4)
    is @$s.sum, 6, 'first  @$s.sum';
    is @$s.sum, 6, 'second @$s.sum (re-iterable)';
}

# 4. Nested iteration like Zef::Pluggable!list-plugins: a Seq element of an
#    array iterated via `@$inner` across two outer passes.
{
    my @backends = ((1, 2).map({ $_ }),);   # one element, itself a Seq
    my @collected;
    for ^2 {
        for @backends -> $backend {
            for @$backend -> $plugin { @collected.push($plugin) }
        }
    }
    is-deeply @collected, [1, 2, 1, 2], 'nested @$backend re-iterates across passes';
}
