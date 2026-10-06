use Test;
use nqp;

# #11209 (ADR-11203 §2.4): the `CStr` REPR, selected by `is repr<CStr>`.
#
# Upstream NativeCall's `explicitly-manage` declares the class from inside
# the sub and boxes the string into it:
#
#     my class CStr is repr<CStr> { method encoding() { $encoding } }
#     $x does ExplicitlyManagedString;
#     $x.cstr = nqp::box_s(nqp::unbox_s($x), CStr)
#
# The object owns a NUL-terminated UTF-8 copy of the string that is never
# freed, so a callee that keeps the pointer (`putenv`) keeps seeing live
# memory. Every expected value here is rakudo's.

plan 23;

my class Callsite is repr<NativeCall> { }
my class CStr is repr<CStr> { method encoding() { 'utf8' } }
my role ExplicitlyManagedString { has $.cstr is rw }

sub build(Str $symbol, @args, %ret) {
    my $site := nqp::create(Callsite);
    my $arg_info := nqp::list();
    for @args -> %arg {
        my $info := nqp::hash();
        nqp::bindkey($info, .key, nqp::decont(.value)) for %arg;
        nqp::push($arg_info, $info);
    }
    my $ret_info := nqp::hash();
    nqp::bindkey($ret_info, .key, nqp::decont(.value)) for %ret;
    nqp::buildnativecall($site, '', nqp::unbox_s($symbol), '', $arg_info, $ret_info);
    $site
}

is CStr.REPR, 'CStr', 'a class declared is repr<CStr> reports it';

my $boxed := nqp::box_s("héllo", CStr);
is $boxed.REPR, 'CStr', 'nqp::box_s into it gives an object of that REPR';
isa-ok $boxed, CStr, 'of the class';
ok $boxed.defined, 'which is concrete';
is $boxed.raku, 'CStr.new', 'and prints as an empty construction, as rakudo does';
is nqp::unbox_s($boxed), 'héllo', 'nqp::unbox_s decodes the C string back';
is nqp::unbox_s(nqp::box_s('', CStr)), '', 'an empty string boxes to an empty C string';

my $other := nqp::box_s("héllo", CStr);
is nqp::unbox_s($other), 'héllo', 'a second box of the same string reads the same';
ok $boxed.WHERE != $other.WHERE, 'and is a separate object';

my $null := nqp::create(CStr);
ok $null.defined, 'nqp::create gives a concrete object';
is $null.REPR, 'CStr', 'of the CStr REPR';
ok nqp::isnull_s(nqp::unbox_s($null)), 'with no C string: unboxing it answers the null str';

# `:encoding` is the class's own method; the VM stores UTF-8 whatever it says.
my class CAscii is repr<CStr> { method encoding() { 'ascii' } }
is nqp::unbox_s(nqp::box_s("héllo", CAscii)), 'héllo', 'an encoding method does not change the stored bytes';

# What `explicitly-manage` does with it.
my $x = 'héllo';
$x does ExplicitlyManagedString;
$x.cstr = nqp::box_s(nqp::unbox_s($x), CStr);
is $x.cstr.REPR, 'CStr', 'the managed string carries a CStr object in its cstr';
is nqp::unbox_s($x.cstr), 'héllo', 'holding the string';
is $x, 'héllo', 'and is still the string';

# Passing it where C wants a char*: the callee gets the object's own buffer.
my $strlen := build('strlen', [{ :type<utf8str>, :free_str(1) },], { :type<ulong> });
is nqp::nativecall(Int, $strlen, nqp::list($x)), 6, 'a managed string is passed as its UTF-8 bytes';
is nqp::nativecall(Int, $strlen, nqp::list('héllo')), 6, 'as a plain Str is';
is nqp::nativecall(Int, $strlen, nqp::list($boxed)), 6, 'and so is a bare CStr object';

# `putenv` keeps the pointer it is given: POSIX makes it part of the
# environment, so the buffer must outlive the call. Same shape as
# nativecall.rakudoc's set_version example.
my $putenv := build('putenv', [{ :type<utf8str>, :free_str(1) },], { :type<int> });
my $getenv := build('getenv', [{ :type<utf8str>, :free_str(1) },], { :type<utf8str>, :free_str(0) });

sub manage(Str $text) {
    my $s = $text;
    $s does ExplicitlyManagedString;
    $s.cstr = nqp::box_s(nqp::unbox_s($s), CStr);
    $s
}

is nqp::nativecall(Int, $putenv, nqp::list(manage('MUTSU_CSTR_A=first'))), 0, 'putenv accepts a managed string';
is nqp::nativecall(Str, $getenv, nqp::list('MUTSU_CSTR_A')), 'first', 'and the retained buffer is still live afterwards';
is nqp::nativecall(Int, $putenv, nqp::list(manage('MUTSU_CSTR_A=second'))), 0, 'a second managed string';
is nqp::nativecall(Str, $getenv, nqp::list('MUTSU_CSTR_A')), 'second', 'replaces the first without disturbing it';
