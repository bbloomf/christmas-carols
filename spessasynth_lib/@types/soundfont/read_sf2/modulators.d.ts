/**
 * Reads the modulator read
 * @param modulatorChunk {RiffChunk}
 * @returns {Modulator[]}
 */
export function readModulators(modulatorChunk: RiffChunk): Modulator[];
export class ReadModulator extends Modulator {
    /**
     * Creates a modulator
     * @param dataArray {IndexedByteArray}
     */
    constructor(dataArray: IndexedByteArray);
}
import { Modulator } from "../basic_soundfont/modulator.js";
import { IndexedByteArray } from "../../utils/indexed_array.js";
