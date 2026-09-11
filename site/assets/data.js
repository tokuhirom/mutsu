// Fetching one of the generated data files under content/.
//
// Every such file is regenerated at deploy time by .github/workflows/pages.yml
// (the batteries and ecosystem manifests, the compatibility stats) or vendored
// from the repository's own corpora (highlights, lessons), and the page that
// reads one changes far less often than the file does. GitHub Pages serves
// everything with a fixed `Cache-Control: max-age=600` and does not let us set
// headers per path, so within ten minutes of a deploy a returning reader can be
// served yesterday's numbers underneath today's page, with no request made at
// all.
//
// `cache: 'no-cache'` is the fix, and it is not `no-store`: the browser still
// stores the response and still reuses it, it just revalidates with the ETag
// first, so an unchanged 300 KB manifest costs a 304 instead of a download.
// One conditional request buys "the data on screen is never staler than the
// page around it".
//
// Versioning the URL instead (`content/x.json?v=<commit>`) cannot do this job:
// the token would have to live in the HTML, which carries the same 600-second
// cache, so a stale shell would simply ask for its stale data by name. The
// shell's own revalidation is the only thing that can refresh the shell, which
// is why this is about the data files and not about cache-busting the site.
//
// Module imports of content/*.js (landing copy, examples, the manual text) are
// deliberately not routed through here: those are source, not measurements, and
// they change with the page that imports them, so the shell's cache window is
// the right granularity for them.
export function fetchData(path) {
  return fetch(path, { cache: 'no-cache' });
}
