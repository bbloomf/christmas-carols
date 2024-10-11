export function getModSourceEnum(curveType: any, polarity: any, direction: any, isCC: any, index: any): number;
export namespace modulatorSources {
    let noController: number;
    let noteOnVelocity: number;
    let noteOnKeyNum: number;
    let polyPressure: number;
    let channelPressure: number;
    let pitchWheel: number;
    let pitchWheelRange: number;
    let link: number;
}
export namespace modulatorCurveTypes {
    export let linear: number;
    export let concave: number;
    export let convex: number;
    let _switch: number;
    export { _switch as switch };
}
export class Modulator {
    /**
     * @param modulator {Modulator}
     * @returns {Modulator}
     */
    static copy(modulator: Modulator): Modulator;
    /**
     * @param mod1 {Modulator}
     * @param mod2 {Modulator}
     * @returns {boolean}
     */
    static isIdentical(mod1: Modulator, mod2: Modulator): boolean;
    /**
     * Creates a modulator
     * @param params {{srcEnum: number, secSrcEnum: number, dest: generatorTypes, amt: number, transform: number}}
     */
    constructor(params: {
        srcEnum: number;
        secSrcEnum: number;
        dest: generatorTypes;
        amt: number;
        transform: number;
    });
    /**
     * The current computed value of this modulator
     * @type {number}
     */
    currentValue: number;
    sourceEnum: number;
    /**
     * @type {generatorTypes}
     */
    modulatorDestination: generatorTypes;
    secondarySourceEnum: number;
    transformAmount: number;
    transformType: number;
    sourcePolarity: number;
    sourceDirection: number;
    sourceUsesCC: number;
    sourceIndex: number;
    sourceCurveType: number;
    secSrcPolarity: number;
    secSrcDirection: number;
    secSrcUsesCC: number;
    secSrcIndex: number;
    secSrcCurveType: number;
    /**
     * Sums transform and creates a NEW modulator
     * @param modulator {Modulator}
     * @returns {Modulator}
     */
    sumTransform(modulator: Modulator): Modulator;
    /**
     * @returns {string}
     */
    debugString(): string;
}
export const DEFAULT_ATTENUATION_MOD_AMOUNT: 960;
export const DEFAULT_ATTENUATION_MOD_CURVE_TYPE: number;
export const defaultModulators: Modulator[];
import { generatorTypes } from "./generator.js";
