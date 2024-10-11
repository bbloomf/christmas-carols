/**
 * Reads the given instrument zone read
 * @param zonesChunk {RiffChunk}
 * @param instrumentGenerators {Generator[]}
 * @param instrumentModulators {Modulator[]}
 * @param instrumentSamples {BasicSample[]}
 * @returns {InstrumentZone[]}
 */
export function readInstrumentZones(zonesChunk: RiffChunk, instrumentGenerators: Generator[], instrumentModulators: Modulator[], instrumentSamples: BasicSample[]): InstrumentZone[];
/**
 * Reads the given preset zone read
 * @param zonesChunk {RiffChunk}
 * @param presetGenerators {Generator[]}
 * @param instruments {Instrument[]}
 * @param presetModulators {Modulator[]}
 * @returns {PresetZone[]}
 */
export function readPresetZones(zonesChunk: RiffChunk, presetGenerators: Generator[], presetModulators: Modulator[], instruments: Instrument[]): PresetZone[];
/**
 * zones.js
 * purpose: reads instrumend and preset zones from soundfont and gets their respective samples and generators and modulators
 */
export class InstrumentZone extends BasicInstrumentZone {
    /**
     * Creates a zone (instrument)
     * @param dataArray {IndexedByteArray}
     */
    constructor(dataArray: IndexedByteArray);
    generatorZoneStartIndex: number;
    modulatorZoneStartIndex: number;
    modulatorZoneSize: number;
    generatorZoneSize: number;
    setZoneSize(modulatorZoneSize: any, generatorZoneSize: any): void;
    /**
     * grab the generators
     * @param generators {Generator[]}
     */
    getGenerators(generators: Generator[]): void;
    /**
     * grab the modulators
     * @param modulators {Modulator[]}
     */
    getModulators(modulators: Modulator[]): void;
    /**
     * Loads the zone's sample
     * @param samples {BasicSample[]}
     */
    getSample(samples: BasicSample[]): void;
    /**
     * Reads the keyRange of the zone
     */
    getKeyRange(): void;
    /**
     * reads the velolicty range of the zone
     */
    getVelRange(): void;
}
export class PresetZone extends BasicPresetZone {
    /**
     * Creates a zone (preset)
     * @param dataArray {IndexedByteArray}
     */
    constructor(dataArray: IndexedByteArray);
    generatorZoneStartIndex: number;
    modulatorZoneStartIndex: number;
    modulatorZoneSize: number;
    generatorZoneSize: number;
    setZoneSize(modulatorZoneSize: any, generatorZoneSize: any): void;
    /**
     * grab the generators
     * @param generators {Generator[]}
     */
    getGenerators(generators: Generator[]): void;
    /**
     * grab the modulators
     * @param modulators {Modulator[]}
     */
    getModulators(modulators: Modulator[]): void;
    /**
     * grab the instrument
     * @param instruments {Instrument[]}
     */
    getInstrument(instruments: Instrument[]): void;
    /**
     * Reads the keyRange of the zone
     */
    getKeyRange(): void;
    /**
     * reads the velolicty range of the zone
     */
    getVelRange(): void;
}
import { RiffChunk } from "../basic_soundfont/riff_chunk.js";
import { Generator } from "../basic_soundfont/generator.js";
import { Modulator } from "../basic_soundfont/modulator.js";
import { Instrument } from "./instruments.js";
import { BasicInstrumentZone } from "../basic_soundfont/basic_zones.js";
import { IndexedByteArray } from "../../utils/indexed_array.js";
import { BasicPresetZone } from "../basic_soundfont/basic_zones.js";
