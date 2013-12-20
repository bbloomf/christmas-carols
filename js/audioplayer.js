$(function(){
  var midiLoaded = false;
  var playUrl = null;
  var playTag = null;
  var _playMIDI = function(url) {
    $('div.player').show(400);
    player = MIDI.Player;
    player.timeWarp = 1; // speed the song is played back
    player.loadFile(url, player.start);
    playUrl = url;
  }
  MIDI.loadPlugin(function(){
    playMIDI = _playMIDI;
    midiLoaded = true;
    if(playUrl) playMIDI(playUrl);
    if(MIDI.loader) MIDI.loader.stop();
    MIDIPlayerPercentage(MIDI.Player);
  });
  var playMIDI = function(url) {
    if(!MIDI.loader) MIDI.loader = new widgets.Loader;
    playUrl = url;
  };
  $('a.play').click(function(e){
    var $this = $(this);
    playMIDI($this.attr('href'));
    $li = $this.parent();
    $li.parents().children().css('background','');
    $li.css('background','lightblue');
    e.preventDefault();
  });

  // Toggle between Pause and Play modes.
  var pausePlayStop = function(stop) {
    var $d = $("#pausePlayStop");
    if (stop) {
      MIDI.Player.stop();
      $d.attr('src',"./images/play.png");
    } else if (MIDI.Player.playing) {
      MIDI.Player.pause(true);
      $d.attr('src',"./images/play.png");
    } else {
      MIDI.Player.resume();
      $d.attr('src',"./images/pause.png");
    }
  };
  window.pausePlayStop = pausePlayStop;

  var MIDIPlayerPercentage = function(player) {
    // update the timestamp
    var $time1 = $("#time1");
    var $time2 = $("#time2");
    var $capsule = $("#capsule");
    var $timeCursor = $("#cursor");
    //
    Event.add($capsule.get(0), "drag", function (event, self) {
      Event.cancel(event);
      player.currentTime = (self.x) / 420 * player.endTime;
      if (player.currentTime < 0) player.currentTime = 0;
      if (player.currentTime > player.endTime) player.currentTime = player.endTime;
      if (self.state === "down") {
        player.pause(true);
      } else if (self.state === "up") {
        player.resume();
      }
    });
    //
    function timeFormatting(n) {
      var minutes = n / 60 >> 0; 
      var seconds = String(n - (minutes * 60) >> 0);
      if (seconds.length == 1) seconds = "0" + seconds;
      return minutes + ":" + seconds;
    };
    player.getNextSong = function(n) {
      $current = $('a.play[href="'+playUrl+'"]').parent();
      $songs = $current.parent().children();
      var id = Math.abs(($songs.index($current) + n) % $songs.length);
      $($songs.get(id)).find('a.play').click();
    };
    player.setAnimation(function(data, element) {
      var percent = data.now / data.end;
      var now = data.now >> 0; // where we are now
      var end = data.end >> 0; // end of song
      if (now === end) { // go to next song
        player.getNextSong(1);
      }
      // display the information to the user
      $timeCursor.width((percent * 100) + "%");
      $time1.html(timeFormatting(now));
      $time2.html("-" + timeFormatting(end - now));
    });
  };
});