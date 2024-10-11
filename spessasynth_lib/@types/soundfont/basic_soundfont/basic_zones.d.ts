export class BasicInstrumentZone extends BasicZone {
    /**
     * Zone's sample. Undefined if global
     * @type {BasicSample|undefined}
     */
    sample: BasicSample | undefined;
    /**
     * The zone's use count
     * @type {number}
     */
    useCount: number;
    deleteZone(): void;
}
export class BasicPresetZone extends BasicZone {
    /**
     * Zone's instrument. Undefined if global
     * @type {BasicInstrument|undefined}
     */
    instrument: BasicInstrument | undefined;
    deleteZone(): void;
}
import { BasicZone } from "./basic_zone.js";
