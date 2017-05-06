
module Dom = Bdom.Dom;

let createCtx (doc: Dom.document) => {
    let canv = Bdom.createCanvas doc 500 500;
    Bdom.addCanvasToBody doc canv;
    Bdom.getContext canv;
};

let looping = ref false;
let rafid = ref 0;

let stopLooping () => looping := false;

let loop (draw: unit => unit) => {
    looping := true;
    Bdom.cancelAnimationFrame !rafid;
    let rec inner _ => {
        draw();
        if !looping {
            rafid := Bdom.requestAnimationFrame inner;
        }
    };
    inner 0.0;
};