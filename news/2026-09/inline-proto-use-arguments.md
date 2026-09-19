# Inline proto declarations in `use` arguments are registered correctly

`use` arguments can include a proto declaration, as in the CLI::Ecosystem 0.0.7
dependency pattern `use CLI::Version ..., proto sub MAIN(|) is export {*}`.
mutsu now parses that declaration as a proto and registers it before invoking the
module's `EXPORT` routine, instead of registering it as an ordinary `MAIN` sub
and reporting a redeclaration when the module defines its multi candidates.

The remaining CLI::Ecosystem load failure is `nqp::gethllsym`, recorded in
[#8775](https://github.com/tokuhirom/mutsu/issues/8775).
