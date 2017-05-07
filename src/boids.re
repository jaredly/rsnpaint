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
  /*boids: [ U.placedBoid 100.0 100.0, U.placedBoid 120.0 100.0, U.placedBoid 160.0 100.0, ],*/
  boids: repeat 100 (fun () => U.randomBoid 1000.0 500.0),
  width: 1000.0,
  height: 500.0
};

let showVector ctx x y (t, v) mul => {
  Dom.Canvas.beginPath ctx;
  Dom.Canvas.moveTo ctx x y;
  let x = x +. (cos t) *. v *. mul;
  let y = y +. (sin t) *. v *. mul;
  Dom.Canvas.lineTo ctx x y;
  Dom.Canvas.stroke ctx;
};

let draw ({ctx, boids, width, height}: state) => {
  Dom.Canvas.setStrokeWidth ctx 1.0;
  Dom.Canvas.clearRect ctx 0.0 0.0 width height;
  /*let boids = List.map (Flocking.boidBehavior2 boids) boids;*/
  /*let boids = List.map (U.moveBoid width height) boids;*/
  let boids = List.map
    (
      fun boid => {
        let (sep, coh, ali, boid) = Flocking.boidBehavior2 boids boid;
        let boid = U.moveBoid width height boid;
        let {x, y, theta} = boid;
        U.triangle ctx x y 8.0 12.0 (theta -. U.pi /. 2.0);
        Dom.Canvas.setFillStyle ctx "rgba(0, 0, 0, 0.05)";
        /*U.circle ctx x y Flocking.neighborhood;*/
        /*U.circle ctx x y (Flocking.avoid_dist /. 2.0);*/
        Dom.Canvas.setStrokeStyle ctx "red";
        /*showVector ctx x y sep 100.0;*/
        Dom.Canvas.setStrokeStyle ctx "green";
        /*showVector ctx x y coh 100.0;*/
        Dom.Canvas.setStrokeStyle ctx "blue";
        /*showVector ctx x y ali 2000.0;*/
        boid
      }
    )
    boids;
  {ctx, boids, width, height}
};