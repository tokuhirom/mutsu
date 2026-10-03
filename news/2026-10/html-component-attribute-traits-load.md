# HTML::Component's modules load: custom attribute traits that re-dispatch to CORE

HTML::Component declares its own attribute trait, `is html-attr`. The trait
re-dispatches to CORE's `trait_mod:<is>($attr, :built)` and then mixes a role
into the attribute. Every tag module that used it died at load time with
"Can't use unknown trait 'is' -> 'html-attr'". Three gaps caused this:

- **CORE's `:rw` and `:built` candidates were not usable from a trait.**
  Calling `trait_mod:<is>($attr, :built)` from a trait found no candidate.
  `:rw` was accepted, but neither one reached the class. Both now record the
  change on the attribute's meta-object, and the class or role declaration
  folds it into the attribute. A private attribute then becomes settable
  through `.new`, and an accessor becomes writable, exactly as if the trait
  were written on the `has` line.
- **A trait imported by a namespaced module was not found from a sibling
  class.** `HTML::Component::Tag::META` imports `is html-attr` and uses it in
  `class HTML::Component::Tag::META-CHARSET`. That class's package chain never
  passes through the module's own package, where the import is recorded.
  Worse, the chain did reach `HTML::Component`, which holds unrelated
  `trait_mod:<is>` candidates, so the lookup stopped there. A declaration now
  tries every package that has a handler, plus the loading module's package,
  until one dispatches.
- **A topic method call in colon form rejected a trailing comma.**
  `.label: :for($id), $text, unless $hide;` failed to parse. The topic form
  kept its own copy of the colon-argument loop. It now shares
  `parse_colon_args` with the other forms, passing its tighter per-argument
  parser, so it gets the trailing-comma, statement-modifier and sequence rules
  too.

Every HTML::Component module that loads under Rakudo now loads under mutsu.
Rendering a page still needs `::?CLASS` to be the consuming class inside a role
body (#11419).
