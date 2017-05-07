type imageElement;

type canvasRenderingContext2D;

type ctx = canvasRenderingContext2D;

type canvasElement;

type document;

type element;

type window;

type event_like 'a;

type keyboardEvent;

external document : document = "" [@@bs.val];

external window : window = "" [@@bs.val];

external getBody : document => element = "body" [@@bs.get];

external appendCanvas : element => canvasElement => unit = "appendChild" [@@bs.send];

external createCanvasElement : document => _ [@bs.as "canvas"] => canvasElement =
  "createElement" [@@bs.send];

external getContext : canvasElement => _ [@bs.as "2d"] => canvasRenderingContext2D =
  "getContext" [@@bs.send];

external createImg : document => _ [@bs.as "img"] => imageElement = "createElement" [@@bs.send];

let addCanvasToBody doc canvas => appendCanvas (getBody doc) canvas;

external requestAnimationFrame : (float => unit) => int = "" [@@bs.val];

external cancelAnimationFrame : int => unit = "" [@@bs.val];

external getElementById : document => string => option element =
  "" [@@bs.return null_to_opt] [@@bs.send];

external addEventListener :
  document => string => (event_like 'a => Js.boolean) => Js.boolean => unit =
  "" [@@bs.send];

external addEventListenerImg :
  imageElement => string => (event_like 'a => Js.boolean) => Js.boolean => unit =
  "addEventListener" [@@bs.send]; /* unsafe casts */

/* canvas api methods */
module Canvas = {
  external setFillStyle : ctx => string => unit = "fillStyle" [@@bs.set];
  external setStrokeStyle : ctx => string => unit = "strokeStyle" [@@bs.set];
  external setStrokeWidth : ctx => float => unit = "lineWidth" [@@bs.set];
  external fillRect : ctx => float => float => float => float => unit = "" [@@bs.send];
  external clearRect : ctx => float => float => float => float => unit = "" [@@bs.send];
  external ellipse : ctx => float => float => float => float => float => float => float => unit =
    "" [@@bs.send];
  external moveTo : ctx => float => float => unit = "" [@@bs.send];
  external lineTo : ctx => float => float => unit = "" [@@bs.send];
  external fill : ctx => unit = "" [@@bs.send];
  external beginPath : ctx => unit = "" [@@bs.send];
  external stroke : ctx => unit = "" [@@bs.send];
};

/* unsafe casts */
external imageElementToJsObj : imageElement => Js.t {..} = "%identity";

external canvasRenderingContext2DToJsObj : canvasRenderingContext2D => Js.t {..} = "%identity";

external canvasElementToJsObj : canvasElement => Js.t {..} = "%identity";

external keyboardEventToJsObj : keyboardEvent => Js.t {..} = "%identity";

external elementToCanvasElement : element => canvasElement = "%identity";

external windowToJsObj : window => Js.t {..} = "%identity";

let createCanvas doc (width: int) (height: int) => {
  let elem = createCanvasElement doc;
  let jscanv = canvasElementToJsObj elem;
  jscanv##width#=width;
  jscanv##height#=height;
  elem
};

let createCtx (doc: document) => {
  let canv = createCanvas doc 500 500;
  addCanvasToBody doc canv;
  getContext canv
};