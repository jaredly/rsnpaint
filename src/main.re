let main () => {
  let ctx = Dom.createCtx Dom.document;
  let state = ref (Boids.initial ctx);
  Animate.loop (fun () => state := Boids.draw !state)
};

main ();