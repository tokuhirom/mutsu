# A module that references its OWN tagged-export subs (`is export(:tag)`) from
# a grammar action method and from an ordinary method. A `use TaggedExportUser`
# without the `:tag` does not import those subs, so the importing program must
# not see them (they are hidden by renaming `GLOBAL::name` -> `MOD::name`). But
# the module's own code still refers to them by bare name and must resolve them.
# Regression pin for T-029 (POFile: grammar action / method could not see a
# module-lexical tagged export).

grammar TaggedExportUser::Parser {
    token TOP { \w+ }
}

class TaggedExportUser::Actions {
    method TOP($/) { make decorate(~$/) }   # tagged export from a grammar action
}

class TaggedExportUser::Widget {
    has $.name;
    method rendered() { decorate($!name) } # tagged export from an ordinary method
}

class TaggedExportUser {
    method parse(Str $input) {
        TaggedExportUser::Parser.parse($input, actions => TaggedExportUser::Actions).made
    }
}

sub decorate(Str $s) is export(:helpers) { "<{$s}>" }
