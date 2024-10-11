/**
 * midi_loader.js
 * purpose: parses a midi file for the seqyencer, including things like marker or CC 2/4 loop detection, copyright detection etc.
 */
export class MIDI extends BasicMIDI {
    /**
     * Parses a given midi file
     * @param arrayBuffer {ArrayBuffer}
     * @param fileName {string} optional, replaces the decoded title if empty
     */
    constructor(arrayBuffer: ArrayBuffer, fileName?: string);
    copyright: any;
    rawMidiName: any;
    midiName: any;
    /**
     * @param fileByteArray {IndexedByteArray}
     * @returns {{type: string, size: number, data: IndexedByteArray}}
     */
    readMIDIChunk(fileByteArray: IndexedByteArray): {
        type: string;
        size: number;
        data: IndexedByteArray;
    };
}
import { BasicMIDI } from "./basic_midi.js";
import { IndexedByteArray } from "../utils/indexed_array.js";
