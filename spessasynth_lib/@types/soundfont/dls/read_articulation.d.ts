/**
 * Reads the articulator chunk
 * @param chunk {RiffChunk}
 * @param disableVibrato {boolean} it seems that dls 1 does not have vibrato lfo, so we shall disable it
 * @returns {{modulators: Modulator[], generators: Generator[]}}
 */
export function readArticulation(chunk: RiffChunk, disableVibrato: boolean): {
    modulators: Modulator[];
    generators: Generator[];
};
import { Modulator } from "../basic_soundfont/modulator.js";
import { Generator } from "../basic_soundfont/generator.js";
