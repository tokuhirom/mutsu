# A ClassHOW whose `add_meta_method` wraps a `populate` metamethod so a call
# with `:rows` reaches the HOW's own method, as Red's MetamodelX::Red::Model
# does for its fixture rows.
unit class MetaWrapHOW is Metamodel::ClassHOW;

has @.added;

method add_meta_method(Mu $target, Str $name, $method) {
    @!added.push: $name;
    if $name eq 'populate' {
        $method.wrap(my method ($how: $model, |arguments) {
            arguments.hash<rows>:exists
                ?? $how.rows-populate($model, |arguments)
                !! callsame
        });
    }
    self.Metamodel::ClassHOW::add_meta_method($target, $name, $method)
}

method rows-populate($model, :@rows) { "rows: {@rows.elems}" }
