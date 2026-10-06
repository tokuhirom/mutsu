use Test;
use nqp;

# #11209 (ADR-11203 §2.4): the `NativeCall` REPR and `is box_target`.
#
# Upstream NativeCall.rakumod builds a routine's call in an attribute of the
# role it mixes into the routine:
#
#     my class Callsite is repr<NativeCall> { }
#     role Native { has Callsite $!call is box_target; ... }
#
# `is box_target` makes the native ops on the object apply to that attribute:
# `nqp::buildnativecall(self, ...)` builds the callsite in `$!call` and
# `nqp::nativecall($rettype, self, $args)` calls it, so `!setup` runs once and
# `nqp::unbox_i($!call)` is non-zero from then on. Every expected value here
# is rakudo's.

plan 19;

my class Callsite is repr<NativeCall> { }

is Callsite.REPR, 'NativeCall', 'a class declared is repr<NativeCall> reports it';
is nqp::create(Callsite).REPR, 'NativeCall', 'and so does an instance';
is nqp::unbox_i(nqp::create(Callsite)), 0, 'an unbuilt callsite unboxes to 0';

# What upstream's `Native` role does, with an `abs` binding to count builds.
my role Dial[$symbol] {
    has Callsite $!call is box_target;
    has Int $!builds = 0;

    method body()   { $!call }
    method built()  { nqp::unbox_i($!call) }
    method builds() { $!builds }

    method setup() {
        return if nqp::unbox_i($!call);
        $!builds++;
        nqp::buildnativecall(
            self, '', nqp::unbox_s($symbol), '',
            nqp::list(nqp::hash('type', 'int')),
            nqp::hash('type', 'int'));
    }

    method dial(Int $n) {
        self.setup;
        nqp::nativecall(Int, self, nqp::list($n))
    }
}

{
    my $abs = sub (Int $n) { $n };
    $abs does Dial['abs'];

    ok $abs.body.defined, 'a box target is a concrete object before anything is built';
    is $abs.body.REPR, 'NativeCall', 'of the declared NativeCall REPR';
    is $abs.built, 0, 'nqp::unbox_i($!call) is 0 until the call is built';

    is $abs.dial(-5), 5, 'nqp::buildnativecall(self) builds the call in the box target';
    ok $abs.built != 0, 'nqp::unbox_i($!call) is non-zero once built';
    is $abs.dial(-7), 7, 'a later call goes through the same callsite';
    is $abs.builds, 1, 'the call was built once, not per invocation';
    isa-ok $abs.body, Callsite, 'the box target is still the Callsite';

    my $strlen = sub (Int $n) { $n };
    $strlen does Dial['strlen'];
    is $strlen.built, 0, 'a second routine starts unbuilt';
    is $abs.dial(-3), 3, 'and the first is unaffected by it';
}

# The same shape on a class, where `self` is a plain instance.
{
    my class Holder {
        has Callsite $!call is box_target;
        method built() { nqp::unbox_i($!call) }
        method build(Str $symbol) {
            nqp::buildnativecall(
                self, '', nqp::unbox_s($symbol), '',
                nqp::list(nqp::hash('type', 'utf8str', 'free_str', 1)),
                nqp::hash('type', 'ulong'));
        }
        method call(Str $s) { nqp::nativecall(Int, self, nqp::list($s)) }
    }

    my $a = Holder.new;
    my $b = Holder.new;
    is $a.built, 0, 'an instance starts with an unbuilt box target';
    $a.build('strlen');
    ok $a.built != 0, 'building through self builds the box target';
    is $a.call('hello'), 5, 'and calling through self calls it';
    is $b.built, 0, 'another instance has a box target of its own';
    $b.build('strlen');
    is $b.call('hi'), 2, 'built and called independently';
}

# A role with no box target leaves the ops on the object itself.
{
    my $site := nqp::create(Callsite);
    nqp::buildnativecall($site, '', 'abs', '',
        nqp::list(nqp::hash('type', 'int')), nqp::hash('type', 'int'));
    is nqp::nativecall(Int, $site, nqp::list(-9)), 9, 'a callsite with no holder is built directly';
}

