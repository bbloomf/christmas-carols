/**
 * Reads the generatorTranslator from the shdr read
 * @param sampleHeadersChunk {RiffChunk}
 * @param smplChunkData {IndexedByteArray|Float32Array}
 * @param isSmplDataRaw {boolean}
 * @returns {LoadedSample[]}
 */
export function readSamples(sampleHeadersChunk: RiffChunk, smplChunkData: IndexedByteArray | Float32Array, isSmplDataRaw?: boolean): LoadedSample[];
export class LoadedSample extends BasicSample {
    /**
     * Creates a sample
     * @param sampleName {string}
     * @param sampleStartIndex {number}
     * @param sampleEndIndex {number}
     * @param sampleLoopStartIndex {number}
     * @param sampleLoopEndIndex {number}
     * @param sampleRate {number}
     * @param samplePitch {number}
     * @param samplePitchCorrection {number}
     * @param sampleLink {number}
     * @param sampleType {number}
     * @param smplArr {IndexedByteArray|Float32Array}
     * @param sampleIndex {number} initial sample index when loading the sfont
     * @param isDataRaw {boolean} if false, the data is decoded as float32.
     * Used for SF2Pack support
     */
    constructor(sampleName: string, sampleStartIndex: number, sampleEndIndex: number, sampleLoopStartIndex: number, sampleLoopEndIndex: number, sampleRate: number, samplePitch: number, samplePitchCorrection: number, sampleLink: number, sampleType: number, smplArr: IndexedByteArray | Float32Array, sampleIndex: number, isDataRaw: boolean);
    sampleStartIndex: number;
    sampleEndIndex: number;
    isSampleLoaded: boolean;
    sampleID: number;
    sampleLength: number;
    sampleDataArray: Float32Array | IndexedByteArray;
    sampleData: Float32Array;
    isDataRaw: boolean;
    /**
     * Get raw data, whether it's compressed or not as we simply write it to the file
     * @return {Uint8Array}
     */
    getRawData(): Uint8Array;
    /**
     * Decode binary vorbis into a float32 pcm
     */
    decodeVorbis(): void;
    /**
     * @returns {Float32Array}
     */
    loadUncompressedData(): Float32Array;
    /**
     * @returns {Float32Array}
     */
    getUncompressedReadyData(): Float32Array;
}
import { RiffChunk } from "../basic_soundfont/riff_chunk.js";
import { IndexedByteArray } from "../../utils/indexed_array.js";
import { BasicSample } from "../basic_soundfont/basic_sample.js";
