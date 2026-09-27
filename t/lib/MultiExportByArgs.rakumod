# A `multi sub EXPORT` whose candidate depends on the `use` arguments, plus a
# lexical class EXPORT constructs on every import. Used by
# t/modules/import-export/multi-export-rerun-dispatches-by-args.t.
my class Token {
    has &!code;
    method new(&code) { my $t := self.bless; $t!set(&code); $t }
    method !set(&c) { &!code := &c }
    method run { &!code() }
}

multi sub EXPORT() {
    Map.new: '&which-export' => { 'no-args ' ~ Token.new({ 'token' }).run }
}
multi sub EXPORT('tagged') {
    Map.new: '&which-export' => { 'tagged ' ~ Token.new({ 'token' }).run }
}
