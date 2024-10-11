export class BasicSample {
    /**
     * The basic representation of a soundfont sample
     * @param sampleName {string}
     * @param sampleRate {number}
     * @param samplePitch {number}
     * @param samplePitchCorrection {number}
     * @param sampleLink {number}
     * @param sampleType {number}
     * @param loopStart {number} relative to sample start
     * @param loopEnd {number} relative to sample start
     */
    constructor(sampleName: string, sampleRate: number, samplePitch: number, samplePitchCorrection: number, sampleLink: number, sampleType: number, loopStart: number, loopEnd: number);
    /**
     * Sample's name
     * @type {string}
     */
    sampleName: string;
    /**
     * Sample rate in Hz
     * @type {number}
     */
    sampleRate: number;
    /**
     * Original pitch of the sample as a MIDI note number
     * @type {number}
     */
    samplePitch: number;
    /**
     * Pitch correction, in cents. Can be negative
     * @type {number}
     */
    samplePitchCorrection: number;
    /**
     * Sample link, currently unused.
     * @type {number}
     */
    sampleLink: number;
    /**
     * Type of the sample, an enum
     * @type {number}
     */
    sampleType: number;
    /**
     * Relative to start of the sample in sample points
     * @type {number}
     */
    sampleLoopStartIndex: number;
    /**
     * Relative to start of the sample in sample points
     * @type {number}
     */
    sampleLoopEndIndex: number;
    /**
     * Indicates if the sample is compressed
     * @type {boolean}
     */
    isCompressed: boolean;
    /**
     * The compressed sample data if it was compressed by spessasynth
     * @type {Uint8Array}
     */
    compressedData: Uint8Array;
    /**
     * The sample's use count
     * @type {number}
     */
    useCount: number;
    /**
     * @returns {Uint8Array|IndexedByteArray}
     */
    getRawData(): Uint8Array | IndexedByteArray;
    /**
     * @param quality {number}
     * @param encodeVorbis {EncodeVorbisFunction}
     */
    compressSample(quality: number, encodeVorbis: EncodeVorbisFunction): void;
    /**
     * @returns {Float32Array}
     */
    getAudioData(): Float32Array;
}
