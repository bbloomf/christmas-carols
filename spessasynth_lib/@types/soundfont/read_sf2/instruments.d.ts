/**
 * Reads the instruments
 * @param instrumentChunk {RiffChunk}
 * @param instrumentZones {InstrumentZone[]}
 * @returns {Instrument[]}
 */
export function readInstruments(instrumentChunk: RiffChunk, instrumentZones: InstrumentZone[]): Instrument[];
/**
 * instrument.js
 * purpose: parses soundfont instrument and stores them as a class
 */
export class Instrument extends BasicInstrument {
    /**
     * Creates an instrument
     * @param instrumentChunk {RiffChunk}
     */
    constructor(instrumentChunk: RiffChunk);
    instrumentZoneIndex: number;
    instrumentZonesAmount: number;
    /**
     * Loads all the instrument zones, given the amount
     * @param amount {number}
     * @param zones {InstrumentZone[]}
     */
    getInstrumentZones(amount: number, zones: InstrumentZone[]): void;
}
import { RiffChunk } from "../basic_soundfont/riff_chunk.js";
import { InstrumentZone } from "./zones.js";
import { BasicInstrument } from "../basic_soundfont/basic_instrument.js";
