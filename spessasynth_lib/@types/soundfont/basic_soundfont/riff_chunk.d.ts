/**
 * @param dataArray {IndexedByteArray}
 * @param readData {boolean}
 * @param forceShift {boolean}
 * @returns {RiffChunk}
 */
export function readRIFFChunk(dataArray: IndexedByteArray, readData?: boolean, forceShift?: boolean): RiffChunk;
/**
 * @param chunk {RiffChunk}
 * @param prepend {IndexedByteArray}
 * @returns {IndexedByteArray}
 */
export function writeRIFFChunk(chunk: RiffChunk, prepend?: IndexedByteArray): IndexedByteArray;
/**
 * @param header {string}
 * @param data {Uint8Array}
 * @param addZeroByte {Boolean}
 * @returns {IndexedByteArray}
 */
export function writeRIFFOddSize(header: string, data: Uint8Array, addZeroByte?: boolean): IndexedByteArray;
/**
 * @param collection {RiffChunk[]}
 * @param type {string}
 * @returns {RiffChunk|undefined}
 */
export function findRIFFListType(collection: RiffChunk[], type: string): RiffChunk | undefined;
/**
 * riff_chunk.js
 * reads a riff read and stores it as a class
 */
export class RiffChunk {
    /**
     * Creates a new riff read
     * @constructor
     * @param header {string}
     * @param size {number}
     * @param data {IndexedByteArray}
     */
    constructor(header: string, size: number, data: IndexedByteArray);
    header: string;
    size: number;
    chunkData: IndexedByteArray;
}
import { IndexedByteArray } from "../../utils/indexed_array.js";
