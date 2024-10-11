/**
 * soundfont.js
 * purpose: parses a soundfont2 file
 */
export class SoundFont2 extends BasicSoundFont {
    /**
     * Initializes a new SoundFont2 Parser and parses the given data array
     * @param arrayBuffer {ArrayBuffer}
     * @param warnDeprecated {boolean}
     */
    constructor(arrayBuffer: ArrayBuffer, warnDeprecated?: boolean);
    dataArray: IndexedByteArray;
    sampleDataStartIndex: number;
    instruments: import("./instruments.js").Instrument[];
    /**
     * @param chunk {RiffChunk}
     * @param expected {string}
     */
    verifyHeader(chunk: RiffChunk, expected: string): void;
    /**
     * @param text {string}
     * @param expected {string}
     */
    verifyText(text: string, expected: string): void;
}
import { BasicSoundFont } from "../basic_soundfont/basic_soundfont.js";
import { IndexedByteArray } from "../../utils/indexed_array.js";
import { RiffChunk } from "../basic_soundfont/riff_chunk.js";
