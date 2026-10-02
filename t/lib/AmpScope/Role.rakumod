use AmpScope::Helpers;

unit role AmpScope::Role is export;

# `&tab-up` names the imported sub, whatever the caller has bound.
method tab-up(|c) { &tab-up(|c) }
method tab-up-ref() { &tab-up.name }
