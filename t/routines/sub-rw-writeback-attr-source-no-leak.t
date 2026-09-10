use Test;

# Sub-call analogue of t/method-rw-writeback-attr-source-no-leak.t.
#
# `apply_rw_bindings_to_env` (src/runtime/types/mod.rs) is the exit-time `is
# rw`/named-container-share writeback for regular SUB/FUNCTION calls -- the
# analogue of the METHOD-call writeback in vm_method_dispatch.rs that used to
# plant an attribute-twigil-shaped pseudo-key (e.g. "%!plugin-config", from a
# named `:$scalar` param bound from an `@`/`%` source that is itself an
# attribute expression like `:%!attr`) into the caller's env, where a
# *different* method's `reconcile_attrs` `:=`-recovery scan could mistake it
# for a real binding and silently overwrite an unrelated instance's own
# same-named attribute (see news/2026-08/rw-writeback-attr-shaped-source-leak.md
# and todo/tickets/sub-rw-writeback-may-also-leak-attr-shaped-source-into-caller-env.md).
#
# Investigated whether the SAME leak reproduces through a plain sub call
# (rather than a method call). It does not: an attribute-shaped named-arg
# source calling a plain sub never reaches bind_function_args_values with a
# populated arg_sources — the light/positional-light call fast paths (or an
# equivalent dispatch shortcut) intervene before the container-share/rw
# writeback bookkeeping runs, so `apply_rw_bindings_to_env` never receives a
# pseudo-key to plant for this shape. This test pins the *observable*
# safety: an attribute-sourced named-scalar sub call must never corrupt an
# unrelated instance's same-named attribute in a later method call. If a
# future change to the sub-call fast paths starts populating rw_bindings for
# this shape, apply_rw_bindings_to_env would need the same attribute-twigil
# guard vm_method_dispatch.rs's rw_writeback loop already has -- this test
# would catch the regression either way.

plan 4;

sub f(:$plugin-config) {
    # Deliberately does NOT rebind -- there is no `:=` binding in this sub
    # body at all, mirroring the method-path test's `copy-with`.
    $plugin-config;
}

class Handler {
    has $.plugin-config = "untouched";

    method copy-with() {
        self.bless(:$!plugin-config);
    }

    method report() {
        $!plugin-config;
    }
}

class Owner {
    has %!plugin-config;

    method configure($k, $v) {
        %!plugin-config{$k} = $v;
    }

    method spawn-all(@handlers) {
        my @out;
        for @handlers -> $h {
            f(:%!plugin-config);
            @out.push: $h.copy-with();
        }
        @out;
    }
}

my $owner = Owner.new;
$owner.configure('k', 'v');

my @handlers = Handler.new(plugin-config => "h1"), Handler.new(plugin-config => "h2");
my @copies = $owner.spawn-all(@handlers);

is @copies[0].report, 'h1', 'first handler keeps its own attribute after a sub call sourced from a %-attr named arg';
is @copies[1].report, 'h2', 'second handler keeps its own attribute, not the owner attribute';
is @handlers[0].plugin-config, 'h1', 'original first handler untouched';
is @handlers[1].plugin-config, 'h2', 'original second handler untouched';
