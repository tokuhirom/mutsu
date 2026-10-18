# `tail` and `head(&callable)` are `Any`'s interpreter twins

The by-name gates that sent every `.tail` and every `.head(&callable)`/`.head(*)` call to the
interpreter's routines are gone (ADR-11276 §9.186): the pure rows answer plain lists, and what they
decline (a lazy list, an unpulled `Seq.new($iterator)`, an instance, a callable count) reaches
`Any`'s interpreter twins through the late MRO walk. Answers are unchanged.
