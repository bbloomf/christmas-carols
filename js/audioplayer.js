import { Sequencer } from "../spessasynth_lib/sequencer/sequencer.js";
import { Synthetizer } from "../spessasynth_lib/synthetizer/synthetizer.js";
import { WORKLET_URL_ABSOLUTE } from "../spessasynth_lib/synthetizer/worklet_url.js";

const sfBuffer = fetch('soundfont.sf3').then(async sf => sf.arrayBuffer());
/**
 * @type Synthetizer
 */
let synth;
/**
 * @type Sequencer
 */
let seq;

function timeFormatting(n) {
  var minutes = n / 60 >> 0; 
  var seconds = String(n - (minutes * 60) >> 0);
  if (seconds.length == 1) seconds = "0" + seconds;
  return minutes + ":" + seconds;
};

$(function(){
  var $time1 = $("#time1");
  var $time2 = $("#time2");
  var $capsule = $("#capsule");
  var $timeCursor = $("#cursor");

  var playUrl = null;
  var playMIDI = async function(url) {
    $('div.player').css({ display: 'flex' });



    // player = MIDI.Player;
    // player.timeWarp = 1; // speed the song is played back
    // player.loadFile(url, player.start);
    const midiFilePromise = fetch(url).then(result => result.arrayBuffer());
    if (!synth) {
      const audioContext = new AudioContext(); // create an audioContext
      await audioContext.audioWorklet.addModule(new URL("../spessasynth_lib/" + WORKLET_URL_ABSOLUTE, import.meta.url)); // add the worklet
      const soundFontArrayBuffer = await sfBuffer;
      synth = new Synthetizer(audioContext.destination, soundFontArrayBuffer); // create the synthetizer    
      seq = new Sequencer([{binary: await midiFilePromise}], synth); // create the sequencer
      seq.loop = false;
      setInterval(updatePosition, 100);
      // synth.worklet.port.addEventListener('message', updatePosition);
    } else {
      seq.loadNewSongList([{binary: await midiFilePromise}]);
      seq.loop = false;
    }
    seq.stop()
    seq.play();
    playUrl = url;
  }
  let lastUpdate = 0;
  const updatePosition = () => {
    const time = Math.floor(seq.currentTime * 10);
    if (lastUpdate === time) return;
    lastUpdate = time;
    var percent = seq.currentTime / seq.duration;
    $timeCursor.width((percent * 100) + "%");
    $time1.html(timeFormatting(seq.currentTime));
    $time2.html("-" + timeFormatting(seq.duration - seq.currentTime));
    if (Math.round(percent * 100) === 100) {
      getNextSong(1);
    }
  };

  $('a.play').click(function(e){
    var $this = $(this);
    playMIDI($this.attr('href'));
    var $li = $this.parent();
    $li.parents().children().css('background','');
    $li.css('background','lightblue');
    e.preventDefault();
  });


  $capsule.on('mousedown mousemove drag', function(e) {
    if (e.buttons === 1) {
      const { x, width } = this.getBoundingClientRect();
      const percent = (e.clientX - x) / width;      
      seq.currentTime = percent * seq.duration;
      $timeCursor.width((percent * 100) + "%");
      e.preventDefault();
    }
  });

  // Toggle between Pause and Play modes.
  var pausePlayStop = function(stop) {
    var $d = $("#pausePlayStop");
    if (stop) {
      seq.stop();
      $('div.player').hide(400);
      $d.attr('src',"./images/play.png");
    } else if (seq.paused) {
      seq.play();
      $d.attr('src',"./images/pause.png");
    } else {
      seq.pause(true);
      $d.attr('src',"./images/play.png");
    }
  };
  var getNextSong = function(n) {
    var $current = $('a.play[href="'+playUrl+'"]').parent();
    var $songs = $current.parent().children();
    var songCount = $songs.length;
    var id = ($songs.index($current) + songCount + n) % songCount;
    $($songs.get(id)).find('a.play').click();
  };
  window.pausePlayStop = pausePlayStop;
  window.getNextSong = getNextSong;
});