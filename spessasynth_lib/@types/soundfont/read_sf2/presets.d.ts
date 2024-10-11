/**
 * Reads the presets
 * @param presetChunk {RiffChunk}
 * @param presetZones {PresetZone[]}
 * @param defaultModulators {Modulator[]}
 * @returns {Preset[]}
 */
export function readPresets(presetChunk: RiffChunk, presetZones: PresetZone[], defaultModulators: Modulator[]): Preset[];
/**
 * parses soundfont presets, also includes function for getting the generators and samples from midi note and velocity
 */
export class Preset extends BasicPreset {
    /**
     * Creates a preset
     * @param presetChunk {RiffChunk}
     * @param defaultModulators {Modulator[]}
     */
    constructor(presetChunk: RiffChunk, defaultModulators: Modulator[]);
    presetZoneStartIndex: number;
    presetZonesAmount: number;
    /**
     * Loads all the preset zones, given the amount
     * @param amount {number}
     * @param zones {PresetZone[]}
     */
    getPresetZones(amount: number, zones: PresetZone[]): void;
}
import { RiffChunk } from "../basic_soundfont/riff_chunk.js";
import { PresetZone } from "./zones.js";
import { BasicPreset } from "../basic_soundfont/basic_preset.js";
