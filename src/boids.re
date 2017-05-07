open BoidUtils.Types;

module U = BoidUtils;

type state = {
  ctx: Dom.ctx,
  boids: list boid,
  width: float,
  height: float,
  goal: (float, float),
  nextGoal: (float, float),
  repel: bool
};

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
  height: 500.0,
  goal: (200.0, 250.0),
  nextGoal: (600.0, 250.0),
  repel: false
};

let showVector ctx x y (t, v) mul => {
  Dom.Canvas.beginPath ctx;
  Dom.Canvas.moveTo ctx x y;
  let x = x +. cos t *. v *. mul;
  let y = y +. sin t *. v *. mul;
  Dom.Canvas.lineTo ctx x y;
  Dom.Canvas.stroke ctx
};

let debug = ref false;

let randomGoal () => (Random.float 1000.0, Random.float 500.0);

let moveGoal p1 p2 => {
  let dst = U.dist p1 p2;
  if (dst < 20.0) {
    (p1, randomGoal ())
  } else {
    let (x, y) = p1;
    let (a, b) = p2;
    let theta = atan2 (b -. y) (a -. x);
    let speed = (max (min 5.0 (dst /. 100.0)) 2.0);
    let dx = (cos theta) *. speed;
    let dy = (sin theta) *. speed;
    ((x +. dx, y +. dy), p2)
  }
};

let draw ({ctx, boids, width, height, goal, nextGoal, repel}: state) => {
  Dom.Canvas.setStrokeWidth ctx 1.0;
  Dom.Canvas.clearRect ctx 0.0 0.0 width height;
  Dom.Canvas.setFillStyle ctx "rgba(0, 0, 255, 0.1)";
  U.circle ctx goal 20.0;
  if (!debug) {
    Dom.Canvas.setStrokeStyle ctx "rgba(0, 0, 255, 0.1)";
    U.circleLine ctx goal 200.0;
  };
  Dom.Canvas.setFillStyle ctx "rgba(0, 255, 0, 0.1)";
  U.circle ctx nextGoal 5.0;
  let (goal, nextGoal) = moveGoal goal nextGoal;
  /*let boids = List.map (Flocking.boidBehavior2 boids) boids;*/
  /*let boids = List.map (U.moveBoid width height) boids;*/
  let boids =
    List.map
      (
        fun boid => {
          let (sep, coh, ali, boid) = Flocking.boidBehavior2 boids boid goal repel;
          let boid = U.moveBoid width height boid;
          let {x, y, theta} = boid;
    Dom.Canvas.setFillStyle ctx "rgba(100, 100, 100, 0.5)";
          U.triangle ctx x y 8.0 12.0 (theta -. U.pi /. 2.0);
          if (!debug) {
            Dom.Canvas.setStrokeStyle ctx "rgba(0, 0, 0, 0.05)";
            U.circleLine ctx (x, y) Flocking.neighborhood;
            U.circleLine ctx (x, y) (Flocking.avoid_dist /. 2.0);
            Dom.Canvas.setStrokeStyle ctx "rgba(255, 0, 0, 0.5)";
            showVector ctx x y sep 100.0;
            Dom.Canvas.setStrokeStyle ctx "rgba(0, 255, 0, 0.5)";
            showVector ctx x y coh 1000.0;
            Dom.Canvas.setStrokeStyle ctx "rgba(0, 0, 255, 0.5)";
            showVector ctx x y ali 1000.0;
          };
          boid
        }
      )
      boids;
  {ctx, boids, width, height, goal, repel, nextGoal}
};