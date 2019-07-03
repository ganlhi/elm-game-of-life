import "elm-canvas/elm-canvas";
import { Elm } from "../src/Main.elm";

Elm.Main.init({
  node: document.querySelector("main"),
  flags: [document.body.offsetWidth, document.body.offsetHeight]
});
