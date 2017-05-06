module Canvas = Bdom.Canvas;

let bounce n dn min max =>
  if (n > max) {
    (-. dn, max)
  } else if (n < min) {
    (-. dn, min)
  } else {
    (dn, n +. dn)
  };

type state = {ctx: Bdom.ctx, x: float, y: float, dx: float, dy: float, ticks: int};

let minx = 78.0;
let miny = 50.0;

let draw {ctx, x, y, dx, dy, ticks} => {
  let (dx, nx) = bounce x dx minx 300.0;
  let (dy, ny) = bounce y dy miny 450.0;
  let hue = mod_float ((float_of_int ticks) /. 10.0) 360.0;
  let saturation = 100.0; /* -. (float_of_int ticks) /. 2000.0 *. 100.0; */
  let color = Printf.sprintf "hsl(%f, %f%%, 50%%)" hue saturation;
  Canvas.setFillStyle ctx color;
  let steps = 5;
  let step = 1.0 /. float_of_int steps;
  let cx = step *. (nx -. x);
  let cy = step *. (ny -. y);
  for i in 0 to steps {
    let i = float_of_int i;
    Canvas.fillRect ctx (x +. cx *. i) (y +. cy *. i) 20.0 10.0
  };
  {ctx, x: nx, dx, y: ny, dy, ticks: ticks + 1}
};

let main () => {
  let ctx = Draw.createCtx Bdom.document;
  Canvas.setFillStyle ctx "black";
  let faster = 3.0;
  let dx = 2.325 *. faster;
  let dy = 1.333 *. faster;
  let state = ref {ctx, x: minx, y: miny, dx, dy, ticks: 0};
  Draw.loop (fun () => state := draw !state)
};

main ();