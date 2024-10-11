export class BasicInstrument {
    /**
     * The instrument's name
     * @type {string}
     */
    instrumentName: string;
    /**
     * The instrument's zones
     * @type {BasicInstrumentZone[]}
     */
    instrumentZones: BasicInstrumentZone[];
    _useCount: number;
    /**
     * @returns {number}
     */
    get useCount(): number;
    addUseCount(): void;
    removeUseCount(): void;
    deleteInstrument(): void;
    /**
     * @param index {number}
     * @returns {boolean} is the zone has been deleted
     */
    safeDeleteZone(index: number): boolean;
    /**
     * @param index {number}
     */
    deleteZone(index: number): void;
}
