$(function(){
  var midiLoaded = false;
  var playUrlOnLoad = null;
  var _playMIDI = function(url) {
    player = MIDI.Player;
    player.timeWarp = 1; // speed the song is played back
    player.loadFile(url, player.start);
  }
  MIDI.loadPlugin(function(){
    playMIDI = _playMIDI;
    midiLoaded = true;
    if(playUrlOnLoad) playMIDI(playUrlOnLoad);
  });
  var playMIDI = function(url) {
    MIDI.loader = new widgets.Loader;
    playUrlOnLoad = url;
  };
  $('a.play').click(function(e){
    playMIDI($(this).attr('href'));
    e.preventDefault();
  });
});