unit module ClassBodyUseImportExporter;

sub greet($name) is export {
    return "Hello, $name!";
}
