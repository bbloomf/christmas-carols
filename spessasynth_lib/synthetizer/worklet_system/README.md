## This the worklet system synthesis folder.

The code here is responsible for a single midi channel, synthesizing the sound to it.

- `worklet_methods` contains the methods for the `main_processor.js`
- `worklet_utilities` contains the various digital signal processing functions such as the wavetable oscillator, low
  pass filter, etc.

`minify_processor.js` uses esbuild to minify the processor code. Importing this instead of `worklet_processor.js` is
recommended.