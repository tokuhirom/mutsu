# A child module's `unit package Parent;` no longer demotes `unit class Parent`

`unit class Async::Workers; use Async::Workers::Msg;` with `Msg` being `unit package Async::Workers;`
left `Async::Workers` registered as a bare package: its `.HOW` was `PackageHOW`, so AttrX::Mooish's
`is mooish` trait refused it ("it is not a Raku role or class") and the module could not load. The
`package` statement now re-opens an existing class's namespace instead of replacing its kind.
