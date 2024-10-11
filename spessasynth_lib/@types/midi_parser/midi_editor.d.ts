/**
 * @param ticks {number}
 * @returns {MidiMessage}
 */
export function getGsOn(ticks: number): MidiMessage;
/**
 * Allows easy editing of the file
 * @param midi {BasicMIDI}
 * @param desiredProgramChanges {{
 *     channel: number,
 *     program: number,
 *     bank: number,
 *     isDrum: boolean
 * }[]} the programs to set on given channels. Note that the channel may be more than 16, function will adjust midi ports automatically
 * @param desiredControllerChanges {{
 *     channel: number,
 *     controllerNumber: number,
 *     controllerValue: number,
 * }[]} the controllers to set on given channels. Note that the channel may be more than 16, function will adjust midi ports automatically
 * @param desiredChannelsToClear {number[]} the channels to remove from the sequence. Note that the channel may be more than 16, function will adjust midi ports automatically
 * @param desiredChannelsToTranspose {{
 *     channel: number,
 *     keyShift: number
 * }[]} the channels to transpose. if keyShift is float, rpn fine tuning will be applied as well. Note that the channel may be more than 16, function will adjust midi ports automatically
 */
export function modifyMIDI(midi: BasicMIDI, desiredProgramChanges?: {
    channel: number;
    program: number;
    bank: number;
    isDrum: boolean;
}[], desiredControllerChanges?: {
    channel: number;
    controllerNumber: number;
    controllerValue: number;
}[], desiredChannelsToClear?: number[], desiredChannelsToTranspose?: {
    channel: number;
    keyShift: number;
}[]): void;
/**
 * Modifies the sequence according to the locked presets and controllers in the given snapshot
 * @param midi {BasicMIDI}
 * @param snapshot {SynthesizerSnapshot}
 */
export function applySnapshotToMIDI(midi: BasicMIDI, snapshot: SynthesizerSnapshot): void;
import { MidiMessage } from "./midi_message.js";
