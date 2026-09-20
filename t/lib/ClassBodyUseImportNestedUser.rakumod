unit module ClassBodyUseImportNestedUser;

class Inner {
    use ClassBodyUseImportExporter;

    method run($x) {
        return greet($x);
    }
}
