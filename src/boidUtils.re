module Types = {
  type boid = {x: float, y: float, theta: float, speed: float, dt: float, dv: float};
};

open Types;

let pi = 3.14159;

let randomBoid width height => {
  x: Random.float width,
  y: Random.float height,
  theta: Random.float pi,
  speed: 0.0,
  dt: 0.0,
  dv: 0.0
};

let s2 x => x *. x;

let rot_around x y cx cy t => {
  let ct = atan2 (y -. cy) (x -. cx);
  let d = sqrt (s2 (x -. cx) +. s2 (y -. cy));
  let nt = ct +. t;
  (cx +. cos nt *. d, cy +. sin nt *. d)
};

let at_rot cx cy dx dy t => {
  let d = sqrt (s2 dx +. s2 dy);
  let ct = atan2 dy dx;
  let nt = ct +. t;
  (cx +. cos nt *. d, cy +. sin nt *. d)
};

let lineTo ctx x y => Dom.Canvas.lineTo ctx (floor x) (floor y);

let triangle ctx x y w h r => {
  /* where x & y are the "center" */
  let hw = w /. 2.0;
  let hh = h /. 2.0;
  let (x1, y1) = at_rot x y (-. hw) (-. hh) r;
  let (x2, y2) = at_rot x y hw (-. hh) r;
  let (x3, y3) = at_rot x y 0.0 hh r;
  Dom.Canvas.beginPath ctx;
  Dom.Canvas.moveTo ctx x1 y1;
  Dom.Canvas.lineTo ctx x2 y2;
  Dom.Canvas.lineTo ctx x3 y3;
  Dom.Canvas.lineTo ctx x1 y1;
  Dom.Canvas.stroke ctx
};

let circle ctx x y r => {
  Dom.Canvas.beginPath ctx;
  Dom.Canvas.ellipse ctx x y r r 0.0 0.0 (2.0 *. pi);
  Dom.Canvas.fill ctx
};

let wrap n max =>
  if (n > max) {
    0.0
  } else if (n < 0.0) {
    max
  } else {
    n
  };

let moveBoid width height {x, y, speed, theta, dt, dv} => {
  let theta = theta +. dt;
  let speed = speed +. dv;
  let x = cos theta *. speed +. x;
  let y = sin theta *. speed +. y;
  let (x, y) = (wrap x width, wrap y height);
  {x, y, speed, theta, dt, dv}
};