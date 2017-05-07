open BoidUtils.Types;

let s2 x => x *. x;

let calcDist {x, y} {x: x2, y: y2} => sqrt (s2 (x2 -. x) +. s2 (y2 -. y));

let tooFar = 100.0;

let far = 60.0;

let tooClose = 20.0;

let vectorFromPos x y => (atan2 y x, sqrt (x *. x +. y *. y));

let posFromVector t v => (cos t *. v, sin t *. v);

let vectorAdd t1 v1 t2 v2 => {
  let (x1, y1) = posFromVector t1 v1;
  let (x2, y2) = posFromVector t2 v2;
  vectorFromPos (x1 +. x2) (y1 +. y2)
};

let pushAway boid {x: ox, y: oy} => {
  let {x, y, theta, speed} = boid;
  let newt = atan2 (y -. oy) (x -. ox);
  let (theta, speed) = vectorAdd theta speed newt 0.2;
  {...boid, theta, speed}
};

let goTowards boid {x: ox, y: oy} => {
  let {x, y, theta, speed} = boid;
  let newt = atan2 (y -. oy) (x -. ox);
  let (theta, speed) = vectorAdd theta speed newt (-0.2);
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
    if (dist < d) {
      (dist, other, boid)
    } else {
      (d, o, boid)
    }
  };

let boidBehavior (boids: list boid) boid => {
  let (dist, closest, _) = List.fold_left findClosest (infinity, List.hd boids, boid) boids;
  print_endline (Printf.sprintf "Foind %f" dist);
  /*limitSpeed (List.fold_left reactToBoid boid boids)*/
  reactToBoid boid closest dist
};