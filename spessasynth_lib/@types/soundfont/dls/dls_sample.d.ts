export class DLSSample extends BasicSample {
    /**
     * @param name {string}
     * @param rate {number}
     * @param pitch {number}
     * @param pitchCorrection {number}
     * @param loopStart {number} sample data points
     * @param loopEnd {number} sample data points
     * @param data {Float32Array}
     * @param sampleDbAttenuation {number} in db
     */
    constructor(name: string, rate: number, pitch: number, pitchCorrection: number, loopStart: number, loopEnd: number, data: Float32Array, sampleDbAttenuation: number);
    /**
     * in decibels of attenuation, WITHOUT EMU CORRECTION
     * @type {number}
     */
    sampleDbAttenuation: number;
    /**
     * @type {Float32Array}
     */
    sampleData: Float32Array;
    getRawData(): Uint8Array;
}
import { BasicSample } from "../basic_soundfont/basic_sample.js";
