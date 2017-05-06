type state = {ctx: Dom.ctx, x: float, y: float, dx: float, dy: float, ticks: int, size: float};

let (minx, miny, maxx, maxy) = (78.0, 50.0, 400.0, 450.0);

let draw {ctx, x, y, dx, dy, ticks, size} => {
  let (dx, nx) = Animate.bounce x dx minx maxx;
  let (dy, ny) = Animate.bounce y dy miny maxy;
  let dy = dy +. 0.1;
  let sintic = sin (float_of_int ticks /. 10.0);
  let hue = mod_float (float_of_int ticks /. 5.0) 360.0;
  let saturation = 70.0;
  let color = Printf.sprintf "hsl(%f, %f%%, 50%%)" hue saturation;
  Dom.Canvas.setFillStyle ctx color;
  let steps = 5;
  let step = 1.0 /. float_of_int steps;
  let cx = step *. (nx -. x);
  let cy = step *. (ny -. y);
  let size = 20.0 +. 10.0 *. sintic;
  let hsize = size /. 2.0;
  for i in 0 to steps {
    let i = float_of_int i;
    Dom.Canvas.fillRect ctx (x +. cx *. i -. hsize) (y +. cy *. i -. hsize) size size
  };
  {ctx, x: nx, dx, y: ny, dy, ticks: ticks + 1, size}
};

let main () => {
  let ctx = Dom.createCtx Dom.document;
  Dom.Canvas.setFillStyle ctx "black";
  let faster = 2.0;
  let dx = 2.325 *. faster;
  let dy = 1.333 *. faster;
  let state = ref {ctx, x: minx, y: miny, dx, dy, ticks: 0, size: 20.0};
  Animate.loop (fun () => state := draw !state)
};

main ();