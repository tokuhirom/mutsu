# A CLI-style module that exports MAIN via `proto … is export` inside a package
# block (the shape zef's Zef::CLI uses). The multi candidates are NOT individually
# marked `is export`; the proto's export covers the whole family.
package ExportedMain {
    proto MAIN(|) is export {*}
    multi sub MAIN('greet', $name) { say "hello $name" }
    multi sub MAIN(Bool :version($) where .so) { say "v9.9" }
}
