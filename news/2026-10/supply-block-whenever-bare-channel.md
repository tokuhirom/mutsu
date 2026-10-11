# `whenever $channel` inside a `supply` block

A bare `Channel` used as a `whenever` source inside `supply { }` was silently
dropped, because only the `react` registration understood a Channel directly. It
is now coerced through `.Supply`, as `whenever` does for any object. Found while
working Log::Timeline from the ecosystem queue; the remaining failure there
(`supply { ... }.Promise` over a channel) is tracked in its own issue.
