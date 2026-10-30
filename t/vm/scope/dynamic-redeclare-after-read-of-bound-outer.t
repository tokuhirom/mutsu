use Test;

# `my $*X := V` in an inner block must make a fresh binding when the same frame
# already read the outer `$*X`, which a caller bound to an immutable value.
# (rakudo's RakuAST::Deparse declares `my $*HEREDOC-INDENT := Str` this way.)
plan 3;

sub inner() { $*H.defined }
sub run($k) {
    my $h := $*H;
    if $k { my $*H := Str; return inner() }
    $h.defined
}
sub outer() { my $*H := 'bound'; run(1) }
is outer(), False, 'the inner declaration shadows the bound outer dynamic';
sub outer2() { my $*H := 'bound'; run(0) }
is outer2(), True, 'the outer binding is untouched when the branch is not taken';

class C {
    method seg($s) { "[{$*Q.defined}]" ~ $s }
    method run(@segs) {
        my @parts;
        for @segs -> $s {
            my $q := $*Q;
            if $s ~~ Str { @parts.push("t{$q.defined}") }
            else { my $*Q := Str; @parts.push(self.seg($s)) }
        }
        @parts.join("|")
    }
    method outer(@segs) { my $*Q := Str; self.run(@segs) }
}
is C.outer(("x", 5)), 'tFalse|[False]5', 'the same shape inside a method with a loop';
