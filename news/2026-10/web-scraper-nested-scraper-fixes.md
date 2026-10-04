# Web::Scraper: nested scrapers no longer share state or crash

Making the `Web::Scraper` distribution's own tests run under mutsu exposed three interpreter bugs, all fixed here:

- `&name` read inside a method that declares `my sub name` resolved to the *caller's* same-named `&name` binding when the method was re-entered from a callback `-> &name { ... }` on another invocant, so the routine ran with the wrong `self`.
- `%.attr = ...` / `@.attr = ...` was a by-name store into whatever container env held under that name; inside a method entered from another instance's method that was the caller's own attribute, so two objects ended up sharing one hash. It now lowers through the accessor, like `$.attr = ...`.
- The closure-return write-back compared caller lexicals with a structural `!=`, which never terminates on object graphs with back references (an XML tree's parent links) and aborted the process with a stack overflow. It compares by identity now.

`t/01_basic.t` now passes. `t/03_chain.t` still fails because a `my $self` in one `my proto` body leaks into another across a nested method call (#11920); `t/02_http.t` needs the network and fails under rakudo too.
