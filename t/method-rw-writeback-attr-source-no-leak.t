use Test;

# A named `:$scalar` param bound from an `@`/`%` SOURCE THAT IS ITSELF AN
# ATTRIBUTE EXPRESSION (`:%!attr`, `:$.attr`) shares the same mutable
# container as the caller's attribute (Slice 2d,
# t/named-param-container-share.t) -- but the caller-side "source name" for
# that sharing is encoded as the attribute's OWN twigil form (e.g.
# "%!plugin-config"), not a genuine lexical the exit-time rw writeback is
# entitled to insert into the caller's env verbatim. Doing so planted a
# pseudo-key that `reconcile_attrs`' `:=`-recovery candidate scan (run at
# every OTHER method's exit) could mistake for a `:=` binding and adopt --
# silently overwriting an unrelated instance's own same-named attribute.
#
# Real-world failure: Cro::HTTP::Router's `RouteSet.definition-complete`
# calls `RouteHandler.copy-adding(..., :%!plugin-config, ...)` (RouteSet's
# OWN `%!plugin-config` Hash attribute) once per included handler; the first
# call's writeback planted "%!plugin-config" in `definition-complete`'s env,
# and the second handler's OWN `plugin-config` attribute (same bare name,
# different sigil, unrelated instance) got silently replaced with RouteSet's
# raw config on its own `.bless` (t/../roast Cro::HTTP suite
# `http-router-plugin.rakutest` "Local configuration in included route
# handler not affected by outer").

plan 4;

class Handler {
    has $.plugin-config = "untouched";

    method copy-with(:$plugin-config) {
        # Deliberately does NOT rebind `$!plugin-config` -- there is no
        # `:=` binding in this method body at all.
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
            @out.push: $h.copy-with(:%!plugin-config);
        }
        @out;
    }
}

my $owner = Owner.new;
$owner.configure('k', 'v');

my @handlers = Handler.new(plugin-config => "h1"), Handler.new(plugin-config => "h2");
my @copies = $owner.spawn-all(@handlers);

is @copies[0].report, 'h1', 'first handler keeps its own attribute after a same-name %-attr named arg';
is @copies[1].report, 'h2', 'second handler keeps its own attribute, not the owner attribute';
is @handlers[0].plugin-config, 'h1', 'original first handler untouched';
is @handlers[1].plugin-config, 'h2', 'original second handler untouched';
