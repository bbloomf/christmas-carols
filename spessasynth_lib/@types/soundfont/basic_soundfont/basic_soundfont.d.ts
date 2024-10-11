export class BasicSoundFont {
    /**
     * Merges soundfonts with the given order. Keep in mind that the info read is copied from the first one
     * @param soundfonts {...BasicSoundFont} the soundfonts to merge, the first overwrites the last
     * @returns {BasicSoundFont}
     */
    static mergeSoundfonts(...soundfonts: BasicSoundFont[]): BasicSoundFont;
    /**
     * Creates a new basic soundfont template
     * @param data {undefined|{presets: BasicPreset[], info: Object<string, string>}}
     */
    constructor(data?: undefined | {
        presets: BasicPreset[];
        info: {
            [x: string]: string;
        };
    });
    /**
     * Soundfont's info stored as name: value. ifil and iver are stored as string representation of float (e.g. 2.1)
     * @type {Object<string, string|IndexedByteArray>}
     */
    soundFontInfo: {
        [x: string]: string | IndexedByteArray;
    };
    /**
     * The soundfont's presets
     * @type {BasicPreset[]}
     */
    presets: BasicPreset[];
    /**
     * The soundfont's samples
     * @type {BasicSample[]}
     */
    samples: BasicSample[];
    /**
     * The soundfont's instruments
     * @type {BasicInstrument[]}
     */
    instruments: BasicInstrument[];
    /**
     * Soundfont's default modulatorss
     * @type {Modulator[]}
     */
    defaultModulators: Modulator[];
    removeUnusedElements(): void;
    /**
     * @param instrument {BasicInstrument}
     */
    deleteInstrument(instrument: BasicInstrument): void;
    /**
     * @param preset {BasicPreset}
     */
    deletePreset(preset: BasicPreset): void;
    /**
     * @param sample {BasicSample}
     */
    deleteSample(sample: BasicSample): void;
    /**
     * To avoid overlapping on multiple desfonts
     * @param offset {number}
     */
    setSampleIDOffset(offset: number): void;
    /**
     * Get the appropriate preset, undefined if not foun d
     * @param bankNr {number}
     * @param programNr {number}
     * @param fallbackToProgram {boolean} if true, if no exact match is found, will use any bank with the given preset
     * @return {BasicPreset}
     */
    getPresetNoFallback(bankNr: number, programNr: number, fallbackToProgram?: boolean): BasicPreset;
    /**
     * Get the appropriate preset
     * @param bankNr {number}
     * @param programNr {number}
     * @returns {BasicPreset}
     */
    getPreset(bankNr: number, programNr: number): BasicPreset;
    /**
     * gets preset by name
     * @param presetName {string}
     * @returns {BasicPreset}
     */
    getPresetByName(presetName: string): BasicPreset;
    write: typeof write;
}
import { Modulator } from "./modulator.js";
import { write } from "./write_sf2/write.js";
