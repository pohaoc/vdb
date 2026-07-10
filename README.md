# vdb — a tiling visual debugger for system performance

vdb is a terminal-based tool built on [bonsai_term](https://github.com/janestreet/bonsai_term) designed to visualize live telemetry and export data for plotting. It features a customizable, tiled workspace where users can create panes driven by custom plugins, allowing them to visually debug performance and resource footprints. Optionally, the data can be recorded and exported for quick plotting.

## Download
```
sudo curl -LsSf https://raw.githubusercontent.com/pohaoc/vdb/main/bin/vdb -o /usr/local/bin/vdb && sudo chmod +x /usr/local/bin/vdb
```


## Build & run

Requires [OxCaml](https://oxcaml.org/)

```sh
opam install bonsai_term
opam install bonsai_term_components
eval $(opam env --switch=5.2.0+ox)
dune build
dune exec ./bin/main.exe
```

## Build a standalone binary

```sh
make dist   # → bin/vdb 
```

## Keybindings

| key | action |
| --- | --- |
| `v` / `s` | split the focused pane vertically (side by side) / horizontally (stacked); the new pane opens a plugin picker |
| `h` `j` `k` `l` | move focus left / down / up / right |
| `H` `J` `K` `L` | move (swap) the focused pane left / down / up / right |
| `<` `>` | shrink / grow the focused pane's width |
| `-` `+` | shrink / grow the focused pane's height |
| `o` | open the focused plugin's options window (only pops up if the plugin declares options; inside: `j`/`k` select, `h`/`l` change, `o`/`Esc` close) |
| `r` | start recording the focused pane's datapoints (only if the plugin is recordable); while recording, the pane's data source is locked
| `Esc` | stop recording → prompts for a save location (relative to the working directory; empty input saves a default-named CSV in the cwd; `Esc` again discards) |
| `x` | close the focused pane |
| `q` / `Ctrl+C` | quit |
| mouse click | focus the clicked pane |

## Plugins
The default ships with cgroup monitoring for now, will be adding gpu profiling next.


## Credits
This is inspired by [sampler](https://github.com/sqshq/sampler). I wanted more custom features for convenience (e.g., recording for dumping data).
