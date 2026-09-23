# The bench dashboard opens on the last 150 commits and can anchor its y-axis at zero

`bench-trend.html` used to open on the whole history. That was a sensible
default while the history was short, but it now spans more than 3,000 commits,
so the recent movement a reader usually comes for is squeezed into the right
edge of each chart. The default window is now the last **150** commits;
`all` and `50` remain one click away, and `#window=0` links to the full history.

The dashboard also gains a **Y axis** control. `fit` (the default, and the only
behaviour before) zooms each chart onto its own data range, so a 2% move is
visible. `from 0` anchors every axis at zero instead, so a chart shows how large
a change is relative to the whole value and a small wobble no longer looks like
a cliff. The choice is part of the URL state (`#y=zero`), so a link carries it,
and it is hidden in the table view, which it does not affect. A zero lower bound
now also formats as `0s` rather than `0.0e+0s`.

Note that the window's default moved, so an old bare link now opens on 150
commits; a link that should show everything needs `#window=0`.
