use Test;

# ADR-0072: a `.throw` several frames below a resume-capable `CATCH` runs that
# handler INLINE at the throw site, so `.resume` continues with the next
# statement of the calling body. The JIT's `CallMethod` shim has to carry the
# same hook as the interpreter's own dispatch arm — a callee hot enough to go
# native otherwise lost the resume, and every statement after the throw in the
# caller's block was skipped even though the handler itself had run.
#
# The warm-up calls are what make the callee a JIT candidate; the file is only
# a real regression test when run under `MUTSU_JIT=on` with a low threshold
# (the `jit-stress` CI job), and passes either way.

plan 6;

class X::Neg is Exception { method message() { "neg" } }

sub guard($v) { $v > 0 ?? $v !! X::Neg.new.throw }

# Warm the callee well past any plausible JIT threshold.
guard($_) for 1..20;

{
    my $caught = False;
    my $reached = False;
    CATCH {
        $caught = True;
        when X::Neg { .resume }
        default { .resume }
    }
    guard(-1);
    $reached = True;
    ok $caught,  'the handler ran for a throw inside a hot callee';
    ok $reached, 'and .resume continued with the next statement';
}

# The same through a subscript assignment, whose ASSIGN-KEY dispatch is a
# method call without a call opcode of its own.
class Guarded {
    has %.store;
    method AT-KEY($key)         { %!store{$key} // 0 }
    method ASSIGN-KEY($key, $v) { $v >= 0 ?? (%!store{$key} = $v) !! X::Neg.new.throw }
}

my $g = Guarded.new;
$g<a> = $_ for 1..20;

{
    my $caught = False;
    my $reached = False;
    CATCH {
        $caught = True;
        when X::Neg { .resume }
        default { .resume }
    }
    $g<a> = -1;
    $reached = True;
    ok $caught,  'the handler ran for a throw inside a hot ASSIGN-KEY';
    ok $reached, 'and .resume continued with the next statement';
    is $g<a>, 20, 'the rejected assignment left the old value';
}

is guard(7), 7, 'the hot callee still answers normally afterwards';

# vim: expandtab shiftwidth=4
