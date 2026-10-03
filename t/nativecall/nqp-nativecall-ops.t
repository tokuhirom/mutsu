use Test;
use nqp;
use NativeCall;

# The six `nqp::` FFI ops upstream NativeCall.rakumod is built on (#11211):
# `buildnativecall` records a call from the argument/return-info hashes that
# `param_hash_for` / `return_hash_for` build, `nativecall` makes it, and
# `nativecallcast` / `nativecallsizeof` / `nativecallglobal` /
# `nativecallrefresh` back `nativecast`, `nativesizeof`, `cglobal` and
# `refresh`. Every expected value here is rakudo's, against libc.

plan 19;

my class Callsite is repr<NativeCall> { }

sub info(%h) {
    my $info := nqp::hash();
    nqp::bindkey($info, .key, nqp::decont(.value)) for %h;
    $info
}

sub build(Str $symbol, @args, %ret) {
    my $site := nqp::create(Callsite);
    my $arg_info := nqp::list();
    nqp::push($arg_info, info($_)) for @args;
    nqp::buildnativecall($site, "", nqp::unbox_s($symbol), "", $arg_info, info(%ret));
    $site
}

{
    my $site := nqp::create(Callsite);
    is nqp::unbox_i($site), 0, 'an unbuilt callsite unboxes to 0';
    is nqp::buildnativecall($site, '', 'abs', '',
        nqp::list(nqp::hash('type', 'int')), nqp::hash('type', 'int')), 0,
        'buildnativecall returns 0';
    ok nqp::unbox_i($site) != 0, 'a built callsite unboxes to non-zero';
    is nqp::nativecall(Int, $site, nqp::list(-5)), 5, 'an int argument and an int return';
}

{
    my $site := build('strlen', [{ :type<utf8str>, :free_str(1) },], { :type<ulong> });
    is nqp::nativecall(Int, $site, nqp::list('hello')), 5, 'a utf8str argument and a ulong return';
}

{
    my $site := build('getenv', [{ :type<utf8str>, :free_str(1) },], { :type<utf8str>, :free_str(0) });
    is nqp::nativecall(Str, $site, nqp::list('PATH')), %*ENV<PATH>, 'a utf8str return';
    my $missing := nqp::nativecall(Str, $site, nqp::list('MUTSU_NQP_NATIVECALL_UNSET'));
    ok $missing =:= Str, 'a NULL char* return is the return type object';
}

{
    my $site := build('srand', [{ :type<uint> },], { :type<void> });
    ok nqp::nativecall(Mu, $site, nqp::list(1)) =:= Mu, 'a void return is the return type object';
}

my $hi;
{
    my $site := build('strdup', [{ :type<utf8str>, :free_str(1) },], { :type<cpointer> });
    $hi = nqp::nativecall(Pointer, $site, nqp::list('hi'));
    isa-ok $hi, Pointer, 'a cpointer return is boxed as the return type';
    ok $hi.Int != 0, 'it holds the C address';
}

is nqp::nativecallcast(Str, Str, nqp::decont($hi)), 'hi', 'nativecallcast to Str reads the string at the address';
is nqp::nativecallcast(int8, Int, nqp::decont($hi)), 104, 'nativecallcast to int8 reads the byte at the address';
isa-ok nqp::nativecallcast(Pointer, Pointer, nqp::decont($hi)), Pointer, 'nativecallcast to Pointer rewraps the address';

is-deeply (nqp::nativecallsizeof(int32), nqp::nativecallsizeof(long),
           nqp::nativecallsizeof(bool), nqp::nativecallsizeof(Pointer)),
          (4, 8, 1, 8), 'nativecallsizeof of native and pointer types';

{
    native halfword is Int is nativesize(16) is repr<P6int> { }
    is nqp::nativecallsizeof(halfword), 2, 'nativecallsizeof of a type declared with is nativesize';
}

ok nqp::nativecallrefresh(nqp::decont($hi)) =:= nqp::decont($hi), 'nativecallrefresh returns its argument';

isa-ok nqp::nativecallglobal('', 'environ', Pointer, Pointer), Pointer, 'nativecallglobal reads a C global';

{
    # A callback argument: libc's qsort calls back into a Raku sub.
    my $callback := nqp::hash('type', 'callback', 'callback_args', nqp::list(
        nqp::hash('type', 'int', 'typeobj', int32),
        nqp::hash('type', 'cpointer', 'typeobj', Pointer),
        nqp::hash('type', 'cpointer', 'typeobj', Pointer)));
    my $site := nqp::create(Callsite);
    nqp::buildnativecall($site, '', 'qsort', '',
        nqp::list(nqp::hash('type', 'vmarray'), nqp::hash('type', 'ulong'),
                  nqp::hash('type', 'ulong'), $callback),
        nqp::hash('type', 'void'));
    my $buf = Buf.new(3, 1, 2);
    my &cmp = sub ($a, $b) { nqp::nativecallcast(int8, Int, nqp::decont($a)) - nqp::nativecallcast(int8, Int, nqp::decont($b)) };
    # Upstream passes a Code argument's `$!do`, as here.
    # TODO: mutsu reads `$!do` as Nil until #11207; the routine itself is the
    # same callable until then.
    my $do := nqp::ifnull(nqp::getattr(nqp::decont(&cmp), Code, q[$!do]), nqp::decont(&cmp));
    nqp::nativecall(Mu, $site, nqp::list($buf, 3, 1, $do));
    is-deeply $buf.list, (1, 2, 3), 'a callback argument is called from C';
}

throws-like { nqp::nativecall(Int, nqp::create(Callsite), nqp::list()) },
    Exception, message => /'not been built'/, 'calling an unbuilt callsite fails loudly';
