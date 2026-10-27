use Test;

# A `my $*OUT` declared inside a method is scoped to that call: once the
# method returns, `print` writes to the caller's $*OUT again. (Red's
# ResultSeq.grep/map install a forbidding $*OUT, then fall back to
# `self.Seq.grep: &filter`, t/41-fallback.)

plan 4;

my @got;
my $*OUT = class :: { method print(*@a) { @got.push(@a.join) } };

class RS {
    method plain() {
        my $*OUT = class :: { method print(|) { die "inner" } };
        5
    }
    method lazy(&f) {
        my $*OUT = class :: { method print(|) { die "inner" } };
        return (1, 2).Seq.grep: &f;
    }
}

RS.plain;
print "a";
is-deeply @got, ["a"], 'print after a method with its own my $*OUT uses the caller $*OUT';

my $r = RS.lazy({ print "b$_"; True });
$r.list.elems;
is-deeply @got, ["a", "b1", "b2"], 'a returned lazy grep prints to the consumer $*OUT';

lives-ok { RS.plain; print "c" }, 'repeat call lives';
is @got.tail, "c", 'and prints to the caller $*OUT';
