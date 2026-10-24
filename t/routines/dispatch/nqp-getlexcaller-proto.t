use Test;
use nqp;

# nqp::getlexcaller finds a dynamic a caller declared in a block, also when
# the call reaches the reader through a proto body's `{*}`. Rakudo's own
# RakuAST::Deparse sets up its dynamics once in `proto method deparse` that
# way and recurses through its multis.

plan 3;

sub c() { nqp::isnull(nqp::getlexcaller('$*INDENT')) ?? 'null' !! 'set' }
sub b() { c() }
sub a() { if True { my $*INDENT = ''; b() } }
is a(), 'set', 'a dynamic declared in a caller\'s block';

my @seen;
class K {
    proto method d(|) {
        if nqp::isnull(nqp::getlexcaller('$*X')) { my $*X = 1; @seen.push('init'); {*} }
        else { @seen.push('nested'); {*} }
    }
    multi method d(Int $n) { $n ?? self.d($n - 1) !! 'done' }
}
is K.d(2), 'done', 'the recursion runs';
is @seen.join(','), 'init,nested,nested', 'only the outermost proto call initializes';
