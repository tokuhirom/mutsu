# Site favicon

The GitHub Pages site (mutsu's home page, manual, playground, ecosystem
listing, etc.) had no favicon: every browser tab showed a generic globe or
blank icon.

Added `site/assets/favicon.svg` — a hexagon (mutsu, 六つ, means "six" in
Japanese) rendered in the site's existing purple-to-pink gradient with an
"m" mark — plus rasterized PNG fallbacks (`favicon-16.png`, `favicon-32.png`,
`apple-touch-icon.png`) and a multi-size `favicon.ico` at the site root for
browsers that don't support SVG favicons. Wired `<link rel="icon">` /
`rel="apple-touch-icon"` tags into every static page under `site/` and into
`scripts/bench-visualize.py`'s `--site-chrome` header, which renders
`bench-trend.html` at deploy time.
