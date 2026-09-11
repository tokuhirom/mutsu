# The site's generated data files revalidate instead of going stale

The site has six fetches of a generated file under `site/content/`: the
batteries and ecosystem manifests, the compatibility stats (twice), the
landing-page highlights and the tutorial lessons. All six were plain `fetch()`
calls with no cache hint, and nothing on the site does URL versioning or
registers a service worker.

What GitHub Pages actually serves (measured on the deployed site):

```
cache-control: max-age=600
etag: "6aa4869a-4da3c"
```

Every path gets the same fixed header — Pages has no way to set headers per
path — so within ten minutes of a deploy a returning reader is served the
previous data with no request made at all, while the HTML around it may already
be the new one. `x-cache: HIT, age: 55` on a second request confirms the Fastly
edge holds the same window, though Pages purges it on publish, so in practice
the residency that matters is the browser's.

All six now go through `site/assets/data.js`:

```js
export function fetchData(path) {
  return fetch(path, { cache: 'no-cache' });
}
```

`no-cache` is not `no-store`: the browser still stores the response and still
reuses it, it just revalidates with the ETag first, so an unchanged 300 KB
manifest costs a 304 rather than a download. One conditional request buys "the
data on screen is never staler than the page around it" — which is the pairing
that matters here, because the manifests are regenerated on every sweep while
the page reading them changes rarely.

Module imports of `content/*.js` (landing copy, examples, the manual text) are
deliberately left alone: those are source rather than measurements, they change
with the page that imports them, and the shell's own cache window is the right
granularity for them.

## Why not version the URLs

`content/x.json?v=<commit>` cannot do this job, and this is the reason worth
recording. The version token would have to live in the HTML, and the HTML
carries the same 600-second cache — so a stale shell would simply ask for its
stale data by name. Only the shell's own revalidation can refresh the shell,
which is why the fix is about how the data files are fetched and not about
cache-busting the site.

## Pinned

`site/e2e.test.mjs` collects every request to `content/*.{json,txt}` across the
whole run and asserts, at the end, that all five files were fetched and each
carried a revalidating directive. Chromium puts the `no-cache` mode on the wire
as `Cache-Control: max-age=0`, on a cold load as well as a warm one, and a bare
`fetch` sends no directive at all — both measured, including a negative check
confirming a bare `fetch` fails the assertion. So a new page that reaches for a
generated file with a plain `fetch` fails the suite rather than silently
inheriting the ten-minute window.
