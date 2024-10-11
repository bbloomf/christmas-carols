export class DLSZone extends BasicInstrumentZone {
    /**
     * @param keyRange {SoundFontRange}
     * @param velRange {SoundFontRange}
     */
    constructor(keyRange: SoundFontRange, velRange: SoundFontRange);
    keyRange: SoundFontRange;
    velRange: SoundFontRange;
    /**
     * @param attenuationCb {number} with EMU correction
     * @param loopingMode {number} the sfont one
     * @param loop {{start: number, end: number}}
     * @param sampleKey {number}
     * @param sample {BasicSample}
     * @param sampleID {number}
     * @param samplePitchCorrection {number} cents
     */
    setWavesample(attenuationCb: number, loopingMode: number, loop: {
        start: number;
        end: number;
    }, sampleKey: number, sample: BasicSample, sampleID: number, samplePitchCorrection: number): void;
}
import { BasicInstrumentZone } from "../basic_soundfont/basic_zones.js";
