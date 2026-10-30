# `require Mod <&multi-sub>` imports a multi-only export

A statement-form `require MultiExp <&crypt>` of a routine the module exports only as `multi`
candidates (no single `Module::crypt` entry) bound `&crypt` to an undefined value, so the call died
with "Unknown function: crypt". The listed name now binds a dispatcher over the module's
candidates, as `use` does.
