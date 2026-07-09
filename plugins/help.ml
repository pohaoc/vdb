open! Core
open Bonsai_term
module Catppuccin = Bonsai_term_catppuccin

let color = Catppuccin.color ~flavor:Catppuccin.Mocha.flavor

let keys =
  [ "v", "split vertically (new pane on the right)"
  ; "s", "split horizontally (new pane below)"
  ; "h j k l", "focus pane left / down / up / right"
  ; "H J K L", "move (swap) pane left / down / up / right"
  ; "< >", "shrink / grow pane width"
  ; "- +", "shrink / grow pane height"
  ; "o", "plugin options (when the plugin has any)"
  ; "r", "record the pane's datapoints (when recordable)"
  ; "Esc", "stop recording and export to CSV"
  ; "x", "close pane"
  ; "q / Ctrl+C", "quit"
  ]
;;

let plugin =
  Vdb.Plugin.of_view
    ~name:"help"
    ~description:"keybindings reference"
    (fun ~dimensions:_ ~focused:_ ~settings:_ (local_ _graph) ->
      Bonsai.return
        (View.vcat
           (View.text ~attrs:[ Attr.bold; Attr.fg (color Lavender) ] "Keybindings"
            :: View.text ""
            :: List.map keys ~f:(fun (key, doc) ->
              View.hcat
                [ View.text
                    ~attrs:[ Attr.bold; Attr.fg (color Sapphire) ]
                    (sprintf "%-12s" key)
                ; View.text ~attrs:[ Attr.fg (color Subtext0) ] doc
                ]))))
;;
