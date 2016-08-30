'use strict'

var elm = Elm.Main.fullscreen(url());

elm.ports.player.subscribe(playSound);

function url() {
    var l = window.location;
    return ((l.protocol === "https:") ? "ws://" : "ws://") + l.host;
    // return ((l.protocol === "https:") ? "wss://" : "ws://") + l.host + l.pathname;
}

// function playSound() {
//     new Audio("http://localhost:4000/sounds/guitar.mp3").play();
// }

function playSound(s) {
    console.log(s)
    new Audio("http://192.168.0.10:4000/sounds/"+s+".mp3").play();
}
