/**
 * generator.js
 * purpose: contains enums for generators and their limis parses reads soundfont generators, sums them and applies limits
 */
/**
 * @param generatorType {number}
 * @param presetGens {Generator[]}
 * @param instrumentGens {Generator[]}
 */
export function addAndClampGenerator(generatorType: number, presetGens: Generator[], instrumentGens: Generator[]): number;
export type generatorTypes = number;
export namespace generatorTypes {
    let INVALID: number;
    let startAddrsOffset: number;
    let endAddrOffset: number;
    let startloopAddrsOffset: number;
    let endloopAddrsOffset: number;
    let startAddrsCoarseOffset: number;
    let modLfoToPitch: number;
    let vibLfoToPitch: number;
    let modEnvToPitch: number;
    let initialFilterFc: number;
    let initialFilterQ: number;
    let modLfoToFilterFc: number;
    let modEnvToFilterFc: number;
    let endAddrsCoarseOffset: number;
    let modLfoToVolume: number;
    let unused1: number;
    let chorusEffectsSend: number;
    let reverbEffectsSend: number;
    let pan: number;
    let unused2: number;
    let unused3: number;
    let unused4: number;
    let delayModLFO: number;
    let freqModLFO: number;
    let delayVibLFO: number;
    let freqVibLFO: number;
    let delayModEnv: number;
    let attackModEnv: number;
    let holdModEnv: number;
    let decayModEnv: number;
    let sustainModEnv: number;
    let releaseModEnv: number;
    let keyNumToModEnvHold: number;
    let keyNumToModEnvDecay: number;
    let delayVolEnv: number;
    let attackVolEnv: number;
    let holdVolEnv: number;
    let decayVolEnv: number;
    let sustainVolEnv: number;
    let releaseVolEnv: number;
    let keyNumToVolEnvHold: number;
    let keyNumToVolEnvDecay: number;
    let instrument: number;
    let reserved1: number;
    let keyRange: number;
    let velRange: number;
    let startloopAddrsCoarseOffset: number;
    let keyNum: number;
    let velocity: number;
    let initialAttenuation: number;
    let reserved2: number;
    let endloopAddrsCoarseOffset: number;
    let coarseTune: number;
    let fineTune: number;
    let sampleID: number;
    let sampleModes: number;
    let reserved3: number;
    let scaleTuning: number;
    let exclusiveClass: number;
    let overridingRootKey: number;
    let unused5: number;
    let endOper: number;
}
/**
 * @type {{min: number, max: number, def: number}[]}
 */
export const generatorLimits: {
    min: number;
    max: number;
    def: number;
}[];
export class Generator {
    /**
     * Constructs a new generator
     * @param type {generatorTypes|number}
     * @param value {number}
     */
    constructor(type?: generatorTypes | number, value?: number);
    /**
     * The generator's enum number
     * @type {generatorTypes|number}
     */
    generatorType: generatorTypes | number;
    /**
     * The generator's 16-bit value
     * @type {number}
     */
    generatorValue: number;
}
