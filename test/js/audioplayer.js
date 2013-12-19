$(function(){
  var playMIDI = function(url) {
    MIDI.loader = new widgets.Loader;
    MIDI.loadPlugin(function(){
      player = MIDI.Player;
      player.timeWarp = 1; // speed the song is played back
      player.loadFile(url, player.start);
      MIDI.loader.stop();
    });
  };
  window.playMIDI = playMIDI;
});