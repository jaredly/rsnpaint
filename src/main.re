module Canvas = Bdom.Canvas;

type state = {ctx: Bdom.ctx, x: float, y: float, dx: float, dy: float};

let bounce n dn min max =>
  if (n > max) {
    (-. dn, max)
  } else if (n < min) {
    (-. dn, min)
  } else {
    (dn, n +. dn)
  };

let draw {ctx, x, y, dx, dy} => {
  let (dx, nx) = bounce x dx 0.0 500.0;
  let (dy, ny) = bounce y dy 0.0 500.0;
  let steps = 5;
  let step = 1.0 /. float_of_int steps;
  let cx = step *. (nx -. x);
  let cy = step *. (ny -. y);
  for i in 0 to steps {
    let i = float_of_int i;
    Canvas.fillRect ctx (x +. cx *. i) (y +. cy *. i) 20.0 10.0
  };
  {ctx, x: nx, dx, y: ny, dy}
};

let main () => {
  let ctx = Draw.createCtx Bdom.document;
  Canvas.setFillStyle ctx "black";
  let faster = 3.0;
  let state = ref {ctx, x: 0.0, y: 0.0, dx: 2.325 *. faster, dy: 1.333 *. faster};
  Draw.loop (fun () => state := draw !state)
};

main ();