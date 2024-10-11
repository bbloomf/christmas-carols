# spessasynth_lib

**A powerful soundfont/MIDI JavaScript library for the browsers.**

```shell
npm install --save spessasynth_lib
```

### [Project site (consider giving it a star!)](https://github.com/spessasus/SpessaSynth)

### [Demo](https://spessasus.github.io/SpessaSynth)

### [Complete documentation](https://github.com/spessasus/SpessaSynth/wiki/Usage-As-Library)

#### Basic example: play a single note

```js
import { Synthetizer } from "spessasynth_lib"

const sfont = await (await fetch("soundfont.sf3")).arrayBuffer();
const ctx = new AudioContext();
// make sure you copied the worklet processor!
await ctx.audioWorklet.addModule("./worklet_processor.min.js");
const synth = new Synthetizer(ctx.destination, sfont);
document.getElementById("button").onclick = async () =>
{
    await ctx.resume();
    synth.programChange(0, 48); // strings ensemble
    synth.noteOn(0, 52, 127);
}
```

## Current Features

### Easy Integration
- **Modular design:** Easy integration into other projects (load what you need)
- **[Detailed documentation:](https://github.com/spessasus/SpessaSynth/wiki/Home)** With [examples!](https://github.com/spessasus/SpessaSynth/wiki/Usage-As-Library#examples)
- **Easy to Use:** basic setup is just [two lines of code!](https://github.com/spessasus/SpessaSynth/wiki/Usage-As-Library#minimal-setup)
- **No dependencies:** _batteries included!_

### Powerful SoundFont Synthesizer
- Suitable for both **real-time** and **offline** synthesis
- **Excellent SoundFont support:**
  - **Full Generator Support**
  - **Full Modulator Support:** *First (to my knowledge) JavaScript SoundFont synth with that feature!*
  - **SoundFont3 Support:** Play compressed SoundFonts!
  - **Experimental SF2Pack Support:** Play soundfonts compressed with BASSMIDI! (*Note: only works with vorbis compression*)
  - **Can load very large SoundFonts:** up to 4GB! *Note: Only Firefox handles this well; Chromium has a hard-coded memory limit*
- **Soundfont manager:** Stack multiple soundfonts!
- **DLS Level 1 and 2 Support:** *internally converted to sf2*
- **Reverb and chorus support:** [customizable!](https://github.com/spessasus/SpessaSynth/wiki/Synthetizer-Class#effects-configuration-object)
- **Export audio files** using [OfflineAudioContext](https://developer.mozilla.org/en-US/docs/Web/API/OfflineAudioContext)
- **[Custom modulators for additional controllers](https://github.com/spessasus/SpessaSynth/wiki/Modulator-Class#default-modulators):** Why not?
- **Written using AudioWorklets:** 
  - Runs in a **separate thread** for maximum performance
  - Supported by all modern browsers
- **Unlimited channel count:** Your CPU is the limit!
- **Excellent MIDI Standards Support:**
  - **MIDI Controller Support:** Default supported controllers [here](https://github.com/spessasus/SpessaSynth/wiki/MIDI-Implementation#supported-controllers)
  - **MIDI Tuning Standard Support:** [more info here](https://github.com/spessasus/SpessaSynth/wiki/MIDI-Implementation#midi-tuning-standard)
  - [Full **RPN** and limited **NRPN** support](https://github.com/spessasus/SpessaSynth/wiki/MIDI-Implementation#supported-registered-parameters)
  - Supports some [**Roland GS** and **Yamaha XG** system exclusives](https://github.com/spessasus/SpessaSynth/wiki/MIDI-Implementation#supported-system-exclusives)

- **High-performance mode:** Play Rush E! _note: may kill your browser ;)_

### Built-in Powerful and Fast Sequencer
- **Supports MIDI formats 0, 1, and 2:** _note: format 2 support is experimental as it's very, very rare_
- **[Multi-Port MIDI](https://github.com/spessasus/SpessaSynth/wiki/About-Multi-Port) support:** More than 16 channels!
- **Smart preloading:** Only preloads the samples used in the MIDI file for smooth playback (down to key and velocity!)
- **Lyrics support:** Add karaoke to your program!
- **Raw lyrics available:** Decode in any encoding! *(Kanji? No problem!)*
- **Runs in Audio Thread as well:** Never blocks the main thread
- **Loop points support:** Ensures seamless loops

### Read and Write SoundFont and MIDI Files with Ease
#### Read and write MIDI files
  - **Smart name detection:** Handles incorrectly formatted and non-standard track names
  - **Raw name available:** Decode in any encoding! *(Kanji? No problem!)*
  - **Port detection during load time:** Manage ports and channels easily!
  - **Used channels on track:** Quickly determine which channels are used
  - **Key range detection:** Detect the key range of the MIDI
  - **Easy MIDI editing:** Use [helper functions](https://github.com/spessasus/SpessaSynth/wiki/Writing-MIDI-Files#modifymidi) to modify the song to your needs!
  - **Loop detection:** Automatically detects loops in MIDIs (e.g., from _Touhou Project_)
  - **First note detection:** Skip unnecessary silence at the start by jumping to the first note!
  - **[Write MIDI files from scratch](https://github.com/spessasus/SpessaSynth/wiki/Creating-MIDI-Files)**
  - **Easy saving:** Save with just [one function!](https://github.com/spessasus/SpessaSynth/wiki/Writing-MIDI-Files#writemidifile)

#### Read and write [RMID files with embedded SF2 soundfonts](https://github.com/spessasus/sf2-rmidi-specification#readme)
  - **[Level 4](https://github.com/spessasus/sf2-rmidi-specification#level-4) compliance:** Reads and writes *everything!*
  - **Compression and trimming support:** Reduce a MIDI file with a 1GB soundfont to **as small as 5MB**!
  - **DLS Version support:** The original legacy format with bank offset detection!
  - **Automatic bank shifting and validation:** Every soundfont *just works!*
  - **Metadata support:** Add title, artist, album name and cover and more! And of course read them too! *(In any encoding!)*
  - **Compatible with [Falcosoft Midi Player 6!](https://falcosoft.hu/softwares.html#midiplayer)**
  - **Easy saving:** [As simple as saving a MIDI file!](https://github.com/spessasus/SpessaSynth/wiki/Writing-MIDI-Files#writermidi)

#### Read and write SoundFont2 files
  - **Easy info access:** Just an [object of strings!](https://github.com/spessasus/SpessaSynth/wiki/SoundFont2-Class#soundfontinfo)
  - **Smart trimming:** Trim the SoundFont to only include samples used in the MIDI *(down to key and velocity!)*
  - **sf3 conversion:** Compress SoundFont2 files to SoundFont3 with variable quality!
  - **Easy saving:** Also just [one function!](https://github.com/spessasus/SpessaSynth/wiki/SoundFont2-Class#write)

#### Read and write SoundFont3 files
  - Same features as SoundFont2 but with now with **Ogg Vorbis compression!**
  - **Variable compression quality:** You choose between file size and quality!
  - **Compression preserving:** Avoid decompressing and recompressing uncompressed samples for minimal quality loss!

#### Read and play DLS Level 1 or 2 files
  - Read DLS (DownLoadable Sounds) files as SF2 files!
  - **Works like a normal soundfont:** *Saving it as sf2 is still [just one function!](https://github.com/spessasus/SpessaSynth/wiki/SoundFont2-Class#write)*
  - Converts articulators to both **modulators** and **generators**!
  - Works with both unsigned 8-bit samples and signed 16-bit samples!
  - **Covers special generator cases:** *such as modLfoToPitch*!
  - **Correct volume:** *looking at you, Viena and gm.sf2!*
  - Support built right into the synthesizer!

### Export MIDI as WAV
  - Save the MIDI file as WAV audio!
  - **Metadata support:** *Embed metadata such as title, artist, album and more!*
  - **Cue points:** *Write MIDI loop points as cue points!*
  - **Loop multiple times:** *Render two (or more) loops into the file for seamless transitions!*
  - *That's right, saving as WAV is also [just one function!](https://github.com/spessasus/SpessaSynth/wiki/Writing-Wave-Files#audiobuffertowav)*


# License

MIT License, except for the stbvorbis_sync.js in the `externals` folder which is licensed under the Apache-2.0 license.