# Hands its MAIN to the importer through the `sub EXPORT` hook (Test::Describe).
multi MAIN() { say "hook main ran" }

sub EXPORT(--> Map()) {
    "&MAIN" => &MAIN,
}
