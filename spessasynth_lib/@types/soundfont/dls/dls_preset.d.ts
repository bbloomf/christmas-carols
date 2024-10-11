export class DLSPreset extends BasicPreset {
    /**
     * Creates a new DLS preset
     * @param ulBank {number}
     * @param ulInstrument {number}
     */
    constructor(ulBank: number, ulInstrument: number);
    DLSInstrument: BasicInstrument;
    presetZones: BasicPresetZone[];
}
import { BasicPreset } from "../basic_soundfont/basic_preset.js";
import { BasicInstrument } from "../basic_soundfont/basic_instrument.js";
import { BasicPresetZone } from "../basic_soundfont/basic_zones.js";
