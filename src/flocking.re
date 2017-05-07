open BoidUtils.Types;

let s2 x => x *. x;

let calcDist {x, y} {x: x2, y: y2} => sqrt (s2 (x2 -. x) +. s2 (y2 -. y));

let calcDir {x, y} {x: x2, y: y2} => atan2 (y2 -. y) (x2 -. x);
let calcDirToPos {x, y} (x2, y2) => atan2 (y2 -. y) (x2 -. x);

let tooFar = 100.0;

let far = 40.0;

let tooClose = 20.0;

let vectorFromPos x y => (atan2 y x, sqrt (x *. x +. y *. y));

let posFromVector t v => (cos t *. v, sin t *. v);

let vectorAdd (t1, v1) (t2, v2) => {
  let (x1, y1) = posFromVector t1 v1;
  let (x2, y2) = posFromVector t2 v2;
  vectorFromPos (x1 +. x2) (y1 +. y2)
};

let pushAway boid {x: ox, y: oy} => {
  let {x, y, theta, speed} = boid;
  let newt = atan2 (y -. oy) (x -. ox);
  let (theta, speed) = vectorAdd (theta, speed) (newt, 0.1);
  {...boid, theta, speed}
};

let goTowards boid {x: ox, y: oy} => {
  let {x, y, theta, speed} = boid;
  let newt = atan2 (y -. oy) (x -. ox);
  let (theta, speed) = vectorAdd (theta, speed) (newt, (-0.05));
  {...boid, theta, speed}
};

let reactToBoid boid other dist =>
  /*let dist = calcDist boid other;*/
  if (dist > tooFar) {
    boid
  } else if (dist > far) {
    goTowards boid other
  } else if (dist < tooClose) {
    boid
    /*pushAway boid other*/
  } else {
    boid
  };

let limit n max =>
  if (n > max) {
    max
  } else {
    n
  };

let limitSpeed boid => {...boid, speed: limit boid.speed 6.0};

let findClosest (d, o, boid) other =>
  if (boid == other) {
    (d, o, boid)
  } else {
    let dist = calcDist boid other;
    if (dist < tooClose) {
      (d, o, pushAway boid other)
    } else if (dist < d && dist > far) {
      (dist, other, boid)
    } else {
      (d, o, boid)
    }
  };

let boidBehavior (boids: list boid) boid => {
  let (dist, closest, _) = List.fold_left findClosest (infinity, List.hd boids, boid) boids;
  /*print_endline (Printf.sprintf "Foind %f" dist);*/
  /*limitSpeed (List.fold_left reactToBoid boid boids)*/
  limitSpeed (reactToBoid boid closest dist)
};

let neighborhood = 100.0;

/* New flocking
   - separation
   - alignment
   - cohesion
   */
let findNeighbors boids boid =>
  List.fold_left
    (
      fun results other =>
        if (other == boid) {
          results
        } else {
          let dist = calcDist boid other;
          if (dist > neighborhood) {
            results
          } else {
            let theta = calcDir boid other;
            [(dist, theta, other), ...results]
          }
        }
    )
    []
    boids;

let avoid_dist = 30.0;

let cohere_min = 60.0;

let align_coeff = 0.001;

let cohere_coeff = 0.0001;

let separate_coeff = 1.0;

let goal_coeff = 0.005;

let vmul n (t, d) => (t, d *. n);

let separate neighbors =>
  List.fold_left
    (
      fun v (d, t, b) => {
        if (d < avoid_dist) {
          vectorAdd v (t, -1.0 /. d)
        } else {
          v
        }
      }
    )
    (0.0, 0.0)
    neighbors |>
  vmul separate_coeff;

let align neighbors => {
  let (n, v) = List.fold_left (fun (n, v) (_, _, boid) => {
    (n + 1, vectorAdd v (boid.theta, boid.speed))
  }) (0, (0.0, 0.0)) neighbors;
  v |> vmul align_coeff |> vmul (if (n > 0) { 1.0 /. (float_of_int n) } else {1.0});
};

let cohere neighbors =>
  List.fold_left (fun v (d, t, _) => {
    if (d > cohere_min) {
      vectorAdd v (t, d /. 3.0)
    } else {
      v
    }
  }) (0.0, 0.0) neighbors |>
  vmul cohere_coeff;

let boidBehavior2 boids boid goal repel => {
  let neighbors = findNeighbors boids boid;
  /*Js.log neighbors;*/
  let sep = separate neighbors;
  let coh = cohere neighbors;
  let ali = align neighbors;
  let go = (calcDirToPos boid goal, if repel { -. goal_coeff } else { goal_coeff });
  let acceleration = sep |> vectorAdd coh |> vectorAdd ali |> vectorAdd go;
  let (theta, speed) = vectorAdd (boid.theta, boid.speed) acceleration;
  (sep, coh, ali, {...boid, theta, speed: limit speed 2.0})
};