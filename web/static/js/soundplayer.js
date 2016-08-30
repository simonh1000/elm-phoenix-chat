'use strict'

var elm = Elm.Main.fullscreen(url());

elm.ports.player.subscribe(playSound);

function url() {
    var l = window.location;
    return ((l.protocol === "https:") ? "ws://" : "ws://") + l.host;
    // return ((l.protocol === "https:") ? "wss://" : "ws://") + l.host + l.pathname;
}

function playSound(s) {
    var l = window.location;
    console.log(l.origin + "/sounds/"+s+".mp3");
    new Audio(l.origin + "/sounds/"+s+".mp3").play();
    // new Audio("http://192.168.0.10:4000/sounds/"+s+".mp3").play();
}
