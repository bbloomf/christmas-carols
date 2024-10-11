export class BasicPreset {
    /**
     * @param modulators {Modulator[]}
     */
    constructor(modulators: Modulator[]);
    /**
     * The preset's name
     * @type {string}
     */
    presetName: string;
    /**
     * The preset's MIDI program number
     * @type {number}
     */
    program: number;
    /**
     * The preset's MIDI bank number
     * @type {number}
     */
    bank: number;
    /**
     * The preset's zones
     * @type {BasicPresetZone[]}
     */
    presetZones: BasicPresetZone[];
    /**
     * SampleID offset for this preset
     * @type {number}
     */
    sampleIDOffset: number;
    /**
     * Stores already found getSamplesAndGenerators for reuse
     * @type {SampleAndGenerators[][][]}
     */
    foundSamplesAndGenerators: SampleAndGenerators[][][];
    /**
     * unused metadata
     * @type {number}
     */
    library: number;
    /**
     * unused metadata
     * @type {number}
     */
    genre: number;
    /**
     * unused metadata
     * @type {number}
     */
    morphology: number;
    /**
     * Default modulators
     * @type {Modulator[]}
     */
    defaultModulators: Modulator[];
    deletePreset(): void;
    /**
     * @param index {number}
     */
    deleteZone(index: number): void;
    /**
     * Preloads all samples (async)
     */
    preload(keyMin: any, keyMax: any): void;
    /**
     * Preloads a specific key/velocity combo
     * @param key {number}
     * @param velocity {number}
     */
    preloadSpecific(key: number, velocity: number): void;
    /**
     * Returns generatorTranslator and generators for given note
     * @param midiNote {number}
     * @param velocity {number}
     * @returns {SampleAndGenerators[]}
     */
    getSamplesAndGenerators(midiNote: number, velocity: number): SampleAndGenerators[];
}
export type SampleAndGenerators = {
    instrumentGenerators: Generator[];
    presetGenerators: Generator[];
    modulators: Modulator[];
    sample: BasicSample;
    sampleID: number;
};
import { Modulator } from "./modulator.js";
