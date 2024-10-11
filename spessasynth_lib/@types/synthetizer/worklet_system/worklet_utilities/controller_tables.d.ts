export const NON_CC_INDEX_OFFSET: 128;
export const CONTROLLER_TABLE_SIZE: 147;
export const resetArray: Int16Array;
export function setResetValue(i: any, v: any): number;
export namespace customControllers {
    let channelTuning: number;
    let channelTransposeFine: number;
    let modulationMultiplier: number;
    let masterTuning: number;
    let channelTuningSemitones: number;
}
export const CUSTOM_CONTROLLER_TABLE_SIZE: number;
export const customResetArray: Float32Array;
export type dataEntryStates = number;
export namespace dataEntryStates {
    let Idle: number;
    let RPCoarse: number;
    let RPFine: number;
    let NRPCoarse: number;
    let NRPFine: number;
    let DataCoarse: number;
    let DataFine: number;
}
