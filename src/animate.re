
let looping = ref false;
let rafid = ref 0;

let stopLooping () => looping := false;

let loop (draw: unit => unit) => {
    looping := true;
    Dom.cancelAnimationFrame !rafid;
    let rec inner _ => {
        draw();
        if !looping {
            rafid := Dom.requestAnimationFrame inner;
        }
    };
    inner 0.0;
};

let bounce n dn min max =>
  if (n > max) {
    (-. dn, max)
  } else if (n < min) {
    (-. dn, min)
  } else {
    (dn, n +. dn)
  };
