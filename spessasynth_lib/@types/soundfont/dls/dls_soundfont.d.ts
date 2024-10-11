export class DLSSoundFont extends BasicSoundFont {
    /**
     * Loads a new DLS (Downloadable sounds) soundfont
     * @param buffer {ArrayBuffer}
     */
    constructor(buffer: ArrayBuffer);
    dataArray: IndexedByteArray;
    instrumentAmount: number;
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
    readDLSInstrumentList: typeof readDLSInstrumentList;
    readDLSInstrument: typeof readDLSInstrument;
    readRegion: typeof readRegion;
    readLart: typeof readLart;
    readDLSSamples: typeof readDLSSamples;
}
import { BasicSoundFont } from "../basic_soundfont/basic_soundfont.js";
import { IndexedByteArray } from "../../utils/indexed_array.js";
import { readDLSInstrumentList } from "./read_instrument_list.js";
import { readDLSInstrument } from "./read_instrument.js";
import { readRegion } from "./read_region.js";
import { readLart } from "./read_lart.js";
import { readDLSSamples } from "./read_samples.js";
