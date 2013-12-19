$(function(){
  var playMIDI = function(url) {
    player.loadFile(url, player.start);
  };
  window.playMIDI = playMIDI;
});