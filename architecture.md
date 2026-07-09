# Architecture
- **`src/pane_tree.ml`** — pure binary tree of panes. Splits carry an orientation and a
  ratio; `layout` tiles a screen region between the leaves, and `neighbor` implements
  geometric (rect-based) navigation used by both focus movement and pane swapping.
- **`src/plugin.ml`** — the plugin interface. A plugin is a name, a description, an
  optional list of `Option_spec`s (key + choices; first choice is the default), and a
  Bonsai component `dimensions -> focused -> settings -> graph -> Output.t Bonsai.t`,
  where `Output.t` is the pane's view, an event handler, and an optional
  `Record_source.t` (CSV axis names + the newest `(x, y)` datapoint) that makes the pane
  recordable. `settings` carries the user's current choice per option key; pressing `o`
  on a pane whose plugin declares options opens a modal window to change them at runtime.
  `recording` is true while the pane is being recorded — plugins that can switch their
  data source (like the cgroup browser) must not switch while it is set.
- **`src/recorder.ml`** — CSV export for recordings: path resolution (relative to the
  working directory, defaulting to `./vdb-<plugin>-<date>-<time>.csv`) and the async
  write. The record buffer itself lives in the app state machine — it is unbounded and
  independent of how much history a plugin displays; the app appends the focused
  source's newest datapoint every time it changes.
- **`src/sampler.ml`** — reusable polling building block: reads a file on an interval,
  parses it to a float, and keeps a rolling window of timestamped samples. The path is
  reactive — when it changes, sampling switches files, and each distinct path keeps its
  own window.
- **`src/picker.ml`** — the built-in "open a plugin" menu shown in newly created panes.
- **`src/app.ml`** — the app itself: a state machine over `{tree; focus; panes}`,
  per-pane component instantiation (`Bonsai.assoc` + `Bonsai.enum` dispatch over the
  registry, so only the plugin a pane displays is active in it), border/title rendering,
  the status bar, and the vim key routing.
- **`plugins/`** — plugins shipped with the binary: `cgroup_mem` (live plot of a
  cgroup-v2 `memory.current`, by default the cgroup vdb itself runs in) and `help`.
  Pressing `b` in a cgroup-memory pane opens an in-pane tree browser of the cgroup
  hierarchy with per-node usage shown inline — arrows move, `→`/`←` expand/collapse,
  Enter watches the selected cgroup, Esc closes.
- **`bin/main.ml`** — assembles the registry and runs the app.
- **`vendor/chart/`** — the line-chart component from `bonsai_term_components` at a
  newer revision than the installed opam snapshot (which only ships `bar_chart`);
  vendored until the opam release catches up.