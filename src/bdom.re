type imageElement;

type canvasRenderingContext2D;
type ctx = canvasRenderingContext2D;

type canvasElement;

module Dom = {
    type document;
    type element;
    type window;
    type event_like 'a;
    type keyboardEvent;
};

external document : Dom.document = "" [@@bs.val];

external window : Dom.window = "" [@@bs.val];

external getBody : Dom.document => Dom.element = "body" [@@bs.get];
external appendCanvas : Dom.element => canvasElement => unit = "appendChild" [@@bs.send];

external createCanvasElement : Dom.document => _ [@bs.as "canvas"] => canvasElement =
  "createElement" [@@bs.send];

external getContext : canvasElement => _ [@bs.as "2d"] => canvasRenderingContext2D =
  "getContext" [@@bs.send];

external createImg : Dom.document => _ [@bs.as "img"] => imageElement =
  "createElement" [@@bs.send];

let addCanvasToBody doc canvas => appendCanvas (getBody doc) canvas;

external requestAnimationFrame : (float => unit) => int = "" [@@bs.val];
external cancelAnimationFrame : int => unit = "" [@@bs.val];


external getElementById : Dom.document => string => option Dom.element =
  "" [@@bs.return null_to_opt] [@@bs.send];

external addEventListener :
  Dom.document => string => (Dom.event_like 'a => Js.boolean) => Js.boolean => unit =
  "" [@@bs.send];

external addEventListenerImg :
  imageElement => string => (Dom.event_like 'a => Js.boolean) => Js.boolean => unit =
  "addEventListener" [@@bs.send]; /* unsafe casts */

/* canvas api methods */
module Canvas = {
    external setFillStyle : ctx => string => unit = "fillStyle" [@@bs.set];
    external setStrokeStyle : ctx => string => unit = "strokeStyle" [@@bs.set];
    external fillRect : ctx => float => float => float => float => unit = "" [@@bs.send];

};

/* unsafe casts */
external imageElementToJsObj : imageElement => Js.t {..} = "%identity";

external canvasRenderingContext2DToJsObj : canvasRenderingContext2D => Js.t {..} =
  "%identity";

external canvasElementToJsObj : canvasElement => Js.t {..} = "%identity";

external keyboardEventToJsObj : Dom.keyboardEvent => Js.t {..} = "%identity";

external elementToCanvasElement : Dom.element => canvasElement = "%identity";

external windowToJsObj : Dom.window => Js.t {..} = "%identity";

let createCanvas doc (width: int) (height: int) => {
    let elem = createCanvasElement doc;
    let jscanv = (canvasElementToJsObj elem);
    jscanv##width #= width;
    jscanv##height #= height;
    elem;
};
