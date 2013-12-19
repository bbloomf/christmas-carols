$(function(){
  var playMIDI = function(url) {
    $.get(url,function(data){
      player = MIDI.Player;
      player.timeWarp = 1; // speed the song is played back
      player.loadFile(data, player.start);
    });
  };
  window.playMIDI = playMIDI;
});