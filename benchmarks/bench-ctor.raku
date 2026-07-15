# Heavy-constructor benchmark: the Zef::Distribution shape. Exercises the
# slow-dispatch construction path that dominated zef's Ecosystems populate
# (PLAN §1 B2): a wide class (20 attributes, @/%-sigil defaults, private
# attributes) inheriting from a parent, `method new(*%_)` delegating to
# `self.bless(|%_, ...)`, and TWEAK submethods at two MRO levels — the child
# one declared `--> Nil` (a definite return spec) with attributive
# parameters. Unlike bench-class.raku (light 3-attr classes, accessor and
# method-call heavy), this stresses bless attribute initialization, submethod
# dispatch, and definite-return finalization.
#
# History: 2026-07-15 this shape was 35x slower than raku; #4557 (accessor
# MRO shadowing), #4558 (per-return string-EVAL of `--> Nil`), and #4561
# (wrap-chain candidate scan on every bless/TWEAK) brought it to ~3x.

class Spec {
    has $.spec;
    submethod TWEAK(:$!spec) { }
}

class Dist is Spec {
    has $.meta-version;
    has $.name;
    has $.auth;
    has $.author;
    has $.authority;
    has $.api;
    has $.ver;
    has $.version;
    has $.description;
    has $.depends;
    has %.provides;
    has %.files;
    has $.source-url;
    has $.license;
    has $.build-depends;
    has $.test-depends;
    has @.resources;
    has %.support;
    has $.builder;
    has %!meta;
    has %.metainfo is rw;

    method new(*%_) { self.bless(|%_, :meta(%_)) }
    submethod TWEAK(:%!meta, :@!resources --> Nil) {
        @!resources = @!resources.map(*.flat);
    }
}

my $checksum = 0;
for 1..5000 -> $i {
    my $d = Dist.new(
        name => "Example::Dist",
        auth => "zef:someone",
        version => "0.0.$i",
        description => "a dist",
        provides => { "Example::Dist" => "lib/Example/Dist.rakumod" },
        depends => ["Other::Dist"],
    );
    $checksum += $d.name.chars;
    $checksum += $d.provides.elems;
}

say "checksum = $checksum";
