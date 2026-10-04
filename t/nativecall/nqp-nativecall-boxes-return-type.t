use Test;
use nqp;

# `nqp::nativecall($rettype, ...)` with a `cpointer` return boxes the address
# as `$rettype` itself when that is a CPointer-REPR class or a mixin of one
# (upstream NativeCall's `Pointer` and `Pointer[T]`), so its methods resolve.
# A NULL return is the type object, as in MoarVM.

plan 6;

class P is repr('CPointer') {
    method Int(P:D:) { nqp::p6box_i(nqp::unbox_i(self)) }
    my role Typed[::T] { method of() { T } }
    method ^parameterize(Mu:U \p, Mu:U \t) {
        my $w := p.^mixin(Typed[t]);
        $w.^set_name("P[{t.^name}]");
        $w
    }
}
my class Callsite is repr('NativeCall') { }

sub build(Str $symbol, @args, $ret) {
    my $site := nqp::create(Callsite);
    my $arg_info := nqp::list();
    nqp::push($arg_info, nqp::hash('type', nqp::unbox_s($_))) for @args;
    nqp::buildnativecall($site, '', nqp::unbox_s($symbol), '', $arg_info,
        nqp::hash('type', nqp::unbox_s($ret)));
    $site
}

my $getenv := build('getenv', ['utf8str'], 'cpointer');

my $p := nqp::nativecall(P, $getenv, nqp::list('PATH'));
is $p.^name, 'P', 'a cpointer return is an instance of the CPointer return class';
ok $p.Int > 0, 'whose own methods see the address';

my $t := nqp::nativecall(P[int8], $getenv, nqp::list('PATH'));
is $t.^name, 'P[int8]', 'a mixin return type boxes as that type';
is $t.of.^name, 'int8', 'and keeps its role methods';
is $t.Int, $p.Int, 'and holds the same address';

my $none := nqp::nativecall(P, $getenv, nqp::list('MUTSU_SURELY_UNSET_VARIABLE'));
ok $none =:= P, 'a NULL return is the type object';
