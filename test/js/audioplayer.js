$(function(){
  var playMIDI = function(url) {
    MIDI.loadPlugin(function(){
      player = MIDI.Player;
      player.timeWarp = 1; // speed the song is played back
      player.loadFile(url, player.start);
    });
  };
  window.playMIDI = playMIDI;
});