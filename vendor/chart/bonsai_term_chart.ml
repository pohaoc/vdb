(** The following modules are for creating charts. *)

module Bar_chart = Bar_chart
module Bar_spec = Bar_chart.Bar_spec

module Subpixel_scatter_chart_collision_resolution =
  Subpixel_scatter_chart_collision_resolution

module Line_chart = Line_chart
module Line_spec = Line_spec
module Scatter_chart = Scatter_chart
module Scatter_chart_collision_resolution = Scatter_chart_collision_resolution
module Scatter_chart_point = Scatter_chart_point
module Scatter_chart_icon = Scatter_chart_icon

(** The following modules are for chart customization. *)

module Axis_labels = Axis_labels
module Axis_range = Axis_range
module Axis_scale = Axis_scale
module Axis_scale_config = Axis_scale_config
module Bar_width_config = Bar_chart.Bar_width_config
module Line_type = Line_type
module Make_label_string = Make_label_string
module Scalar_axis_labels_config = Scalar_axis_labels_config
module Theme = Theme

module For_testing = struct
  module Axis_title = Axis_title
  module Braille_dots = Braille_dots
  module Dots = Dots
  module Graph = Graph
  module Graph_title = Graph_title
  module Label = Label
  module Line = Line
end
