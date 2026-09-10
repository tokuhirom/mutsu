use v6;
use Test;

plan 7;

# A routine that recurses from *inside* its own `for`/`while` loop must not let
# the recursive callee clobber the caller's loop accumulator or loop parameter.
# Both the compiled VM path and the interpreter (sub-with-inner-sub / module)
# path are exercised.

# 1. Accumulator survives recursion inside the loop body (VM path).
{
    my $n = 0;
    sub acc() {
        $n = $n + 1;
        my $r = 0;
        for ^2 -> $i {
            acc() if $n < 2;
            $r = $r + 100;
        }
        return $r;
    }
    is acc(), 200, 'accumulator survives in-loop recursion';
}

# 2. Nested hash build via recursion (the system-collapse shape, VM path).
{
    sub collapse($data) {
        return $data unless $data ~~ Hash;
        my %return;
        for $data.keys -> $idx {
            given $idx {
                when /^ 'stop' / { return "RESOLVED"; }
                default { %return{$idx} = collapse($data{$idx}); }
            }
        }
        return %return;
    }
    my %d = (a => "x", b => { "stop.x" => "deep" });
    my %got = collapse(%d);
    is %got<a>, "x", 'outer key a preserved';
    is %got<b>, "RESOLVED", 'nested key b resolved (loop param not leaked)';
}

# 3. Loop parameter is not leaked across recursion (both keys correct).
{
    sub keys-ok($data) {
        return $data unless $data ~~ Hash;
        my %out;
        for $data.keys.sort -> $k {
            given $k {
                when /^ 'zap' / { return "Z"; }
                default { %out{$k} = keys-ok($data{$k}); }
            }
        }
        return %out;
    }
    my %d = (name => { "zap.q" => 1 }, from => "native");
    my %got = keys-ok(%d);
    is %got<name>, "Z", 'key "name" not corrupted to inner loop key';
    is %got<from>, "native", 'sibling key "from" preserved';
}

# 4. Same, but the routine declares an inner `my sub` (routes through the
#    interpreter path instead of the compiled VM path).
{
    sub collapse2($data) {
        return $data unless $data ~~ Hash;
        my sub helper($x) { return $x }
        my $return = $data.WHAT.new;
        for $data.keys -> $idx {
            given $idx {
                when /^ 'stop' / {
                    my $h = helper($idx);
                    return "RESOLVED";
                }
                default {
                    my $val = collapse2($data{$idx});
                    $return{$idx} = $val if $return ~~ Hash;
                }
            }
        }
        return $return;
    }
    my %d = (a => "x", name => { "stop.x" => "deep" });
    my %got = collapse2(%d);
    is %got<a>, "x", 'interpreter path: outer key a preserved';
    is %got<name>, "RESOLVED", 'interpreter path: key "name" not corrupted';
}

