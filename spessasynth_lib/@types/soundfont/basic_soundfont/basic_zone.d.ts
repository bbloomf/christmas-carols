/**
 * @typedef {Object} SoundFontRange
 * @property {number} min - the minimum midi note
 * @property {number} max - the maximum midi note
 */
export class BasicZone {
    /**
     * The zone's velocity range
     * @type {SoundFontRange}
     */
    velRange: SoundFontRange;
    /**
     * The zone's key range
     * @type {SoundFontRange}
     */
    keyRange: SoundFontRange;
    /**
     * Indicates if the zone is global
     * @type {boolean}
     */
    isGlobal: boolean;
    /**
     * The zone's generators
     * @type {Generator[]}
     */
    generators: Generator[];
    /**
     * The zone's modulators
     * @type {Modulator[]}
     */
    modulators: Modulator[];
}
export type SoundFontRange = {
    /**
     * - the minimum midi note
     */
    min: number;
    /**
     * - the maximum midi note
     */
    max: number;
};
