module Axis = struct
  type t =
    { min : float
    ; max : float
    ; label : String.t
    }

  let create ~min ~max ~label =
    assert (min <= max);
    { min; max; label }
  ;;
end

module Data = struct
  type t =
    { xdata : float Array.t
    ; ydata : float Array.t
    }

  let create x y = { xdata = x; ydata = y }
end

let init ?(device = "qtwidget") () =
  Plplot.plsdev device;
  Plplot.plinit ()
;;

let finish () = Plplot.plend ()

let plot (xaxis : Axis.t) (yaxis : Axis.t) (data : Data.t) title =
  Plplot.plenv xaxis.min xaxis.max yaxis.min yaxis.max 0 0;
  Plplot.pllab xaxis.label yaxis.label title;
  Plplot.plline data.xdata data.ydata
;;
