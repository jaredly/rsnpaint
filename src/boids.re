open BoidUtils.Types;

module U = BoidUtils;

type state = {ctx: Dom.ctx, boids: list boid, width: float, height: float};

let rec repeat n fn =>
  switch n {
  | 0 => []
  | _ => [fn (), ...repeat (n - 1) fn]
  };

Random.self_init ();

let initial ctx => {
  ctx,
  boids: repeat 5 (fun () => U.randomBoid 500.0 500.0),
  width: 500.0,
  height: 500.0
};

let draw ({ctx, boids, width, height}: state) => {
  Dom.Canvas.setStrokeWidth ctx 1.0;
  Dom.Canvas.clearRect ctx 0.0 0.0 width height;
  let boids = List.map (Flocking.boidBehavior boids) boids;
  let boids = List.map (U.moveBoid width height) boids;
  List.iter
    (
      fun boid => {
        let {x, y, theta} = boid;
        U.triangle ctx x y 8.0 12.0 (theta -. U.pi /. 2.0);
        Dom.Canvas.setFillStyle ctx "rgba(0, 0, 0, 0.05)";
        U.circle ctx x y Flocking.tooFar;
        U.circle ctx x y Flocking.far;
        U.circle ctx x y Flocking.tooClose
      }
    )
    boids;
  {ctx, boids, width, height}
};