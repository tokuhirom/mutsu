use v6.e.PREVIEW;
# A declarator chosen by the `use` arguments: the EXPORT hook binds
# `LEXICAL::<EXPORTHOW>` for the importer (annotations' `annotation`, #15259).
my role AnnotatedHOW[::H] is H {
    has @!notes;
    method compose(Mu \type, |) { my \r = callsame; @!notes.push: type.^name; r }
    method notes(::?ROLE:D:) { @!notes }
}
my enum Mode (:DECLARE<declare>, :SUPERSEDE<supersede>);
my subset Known of Str:D where { $*LANG andthen .know_how: $^how };
my package EXPORTHOW { }
my constant EMPTY = Map.new;
only MAKEPKG($name) is raw { Metamodel::PackageHOW.new_type: :$name }
only BINDPKG(Mu \a, Mu \b) is raw { a.WHO.{b.^shortname} := b }
only PACKAGE(@names where 2 <= *, Mu \final) is raw {
    my @pkgs := cache @names.head(*.pred).produce(* ~ '::' ~ *).map(&MAKEPKG);
    @pkgs.reduce(&BINDPKG).WHO.{@names.tail} := final;
    @pkgs[0]
}
proto EXPORT(+ --> Map:D) {*}
multi EXPORT(+ [Mode(Str:D) $mode, Known $base]) { samewith $mode, $base }
multi EXPORT(DECLARE, Str:D $base --> EMPTY) {
    LEXICAL::<EXPORTHOW> := PACKAGE <EXPORTHOW DECLARE noted>, AnnotatedHOW[$*LANG.how: $base];
}
