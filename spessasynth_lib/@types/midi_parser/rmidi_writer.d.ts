/**
 * @typedef {Object} RMIDMetadata
 * @property {string|undefined} name - the name of the file
 * @property {string|undefined} engineer - the engineer who worked on the file
 * @property {string|undefined} artist - the artist
 * @property {string|undefined} album - the album
 * @property {string|undefined} genre - the genre of the song
 * @property {ArrayBuffer|undefined} picture - the image for the file (album cover)
 * @property {string|undefined} comment - the coment of the file
 * @property {string|undefined} creationDate - the creation date of the file
 * @property {string|undefined} copyright - the copyright of the file
 * @property {string|unescape} midiEncoding - the encoding of the inner MIDI file
 */
/**
 * Writes an RMIDI file
 * @param soundfontBinary {Uint8Array}
 * @param mid {BasicMIDI}
 * @param soundfont {BasicSoundFont}
 * @param bankOffset {number} the bank offset for RMIDI
 * @param encoding {string} the encoding of the RMIDI info chunk
 * @param metadata {RMIDMetadata} the metadata of the file. Optional. If provided, the encoding is forced to utf-8/
 * @param correctBankOffset {boolean}
 * @returns {IndexedByteArray}
 */
export function writeRMIDI(soundfontBinary: Uint8Array, mid: BasicMIDI, soundfont: BasicSoundFont, bankOffset?: number, encoding?: string, metadata?: RMIDMetadata, correctBankOffset?: boolean): IndexedByteArray;
export type RMIDINFOChunks = string;
export namespace RMIDINFOChunks {
    let name: string;
    let album: string;
    let artist: string;
    let genre: string;
    let picture: string;
    let copyright: string;
    let creationDate: string;
    let comment: string;
    let engineer: string;
    let software: string;
    let encoding: string;
    let midiEncoding: string;
    let bankOffset: string;
}
export type RMIDMetadata = {
    /**
     * - the name of the file
     */
    name: string | undefined;
    /**
     * - the engineer who worked on the file
     */
    engineer: string | undefined;
    /**
     * - the artist
     */
    artist: string | undefined;
    /**
     * - the album
     */
    album: string | undefined;
    /**
     * - the genre of the song
     */
    genre: string | undefined;
    /**
     * - the image for the file (album cover)
     */
    picture: ArrayBuffer | undefined;
    /**
     * - the coment of the file
     */
    comment: string | undefined;
    /**
     * - the creation date of the file
     */
    creationDate: string | undefined;
    /**
     * - the copyright of the file
     */
    copyright: string | undefined;
    /**
     * - the encoding of the inner MIDI file
     */
    midiEncoding: string | typeof unescape;
};
import { IndexedByteArray } from "../utils/indexed_array.js";
