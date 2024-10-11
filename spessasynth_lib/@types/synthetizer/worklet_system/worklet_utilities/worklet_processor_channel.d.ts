/**
 * @typedef {Object} WorkletProcessorChannel
 * @property {Int16Array} midiControllers - array of MIDI controller values + the values used by modulators as source (pitch bend, bend range etc.)
 * @property {boolean[]} lockedControllers - array indicating if a controller is locked
 * @property {Float32Array} customControllers - array of custom (not sf2) control values such as RPN pitch tuning, transpose, modulation depth, etc.
 *
 * @property {number} channelTransposeKeyShift - key shift of the channel
 * @property {Int8Array} channelOctaveTuning - the tuning for octave on this channel
 * @property {Int16Array} keyCentTuning - tuning of individual keys in cents
 * @property {boolean} holdPedal - indicates whether the hold pedal is active
 * @property {boolean} drumChannel - indicates whether the channel is a drum channel
 * @property {number} velocityOverride - overrides velocity if > 0 otherwise disabled
 *
 * @property {dataEntryStates} dataEntryState - the current state of the data entry
 * @property {number} NRPCoarse - the current coarse value of the Non-Registered Parameter
 * @property {number} NRPFine - the current fine value of the Non-Registered Parameter
 * @property {number} RPValue - the current value of the Registered Parameter
 *
 * @property {BasicPreset} preset - the channel's preset
 * @property {boolean} lockPreset - indicates whether the program on the channel is locked
 * @property {boolean} presetUsesOverride - indcates if the channel uses a preset from the override soundfont.
 *
 * @property {boolean} lockGSNRPNParams - indicates whether the GS NRPN parameters are enabled
 * @property {Object} channelVibrato - vibrato settings for the channel
 * @property {number} channelVibrato.depth - depth of the vibrato effect (cents)
 * @property {number} channelVibrato.delay - delay before the vibrato effect starts (seconds)
 * @property {number} channelVibrato.rate - rate of the vibrato oscillation (Hz)
 
 * @property {boolean} isMuted - indicates whether the channel is muted
 * @property {WorkletVoice[]} voices - array of voices currently active on the channel
 * @property {WorkletVoice[]} sustainedVoices - array of voices that are sustained on the channel
 * @property {WorkletVoice[][][]} cachedVoices - first is midi note, second is velocity. output is an array of WorkletVoices
 */
/**
 * @param sendEvent {boolean}
 * @this {SpessaSynthProcessor}
 */
export function createWorkletChannel(this: SpessaSynthProcessor, sendEvent?: boolean): void;
/**
 * This is a channel configuration enum, it is internally sent from Synthetizer via controller change
 */
export type channelConfiguration = number;
export namespace channelConfiguration {
    let velocityOverride: number;
}
export type WorkletProcessorChannel = {
    /**
     * - array of MIDI controller values + the values used by modulators as source (pitch bend, bend range etc.)
     */
    midiControllers: Int16Array;
    /**
     * - array indicating if a controller is locked
     */
    lockedControllers: boolean[];
    /**
     * - array of custom (not sf2) control values such as RPN pitch tuning, transpose, modulation depth, etc.
     */
    customControllers: Float32Array;
    /**
     * - key shift of the channel
     */
    channelTransposeKeyShift: number;
    /**
     * - the tuning for octave on this channel
     */
    channelOctaveTuning: Int8Array;
    /**
     * - tuning of individual keys in cents
     */
    keyCentTuning: Int16Array;
    /**
     * - indicates whether the hold pedal is active
     */
    holdPedal: boolean;
    /**
     * - indicates whether the channel is a drum channel
     */
    drumChannel: boolean;
    /**
     * - overrides velocity if > 0 otherwise disabled
     */
    velocityOverride: number;
    /**
     * - the current state of the data entry
     */
    dataEntryState: dataEntryStates;
    /**
     * - the current coarse value of the Non-Registered Parameter
     */
    NRPCoarse: number;
    /**
     * - the current fine value of the Non-Registered Parameter
     */
    NRPFine: number;
    /**
     * - the current value of the Registered Parameter
     */
    RPValue: number;
    /**
     * - the channel's preset
     */
    preset: BasicPreset;
    /**
     * - indicates whether the program on the channel is locked
     */
    lockPreset: boolean;
    /**
     * - indcates if the channel uses a preset from the override soundfont.
     */
    presetUsesOverride: boolean;
    /**
     * - indicates whether the GS NRPN parameters are enabled
     */
    lockGSNRPNParams: boolean;
    /**
     * - vibrato settings for the channel
     */
    channelVibrato: {
        depth: number;
        delay: number;
        rate: number;
    };
    /**
     * - indicates whether the channel is muted
     */
    isMuted: boolean;
    /**
     * - array of voices currently active on the channel
     */
    voices: WorkletVoice[];
    /**
     * - array of voices that are sustained on the channel
     */
    sustainedVoices: WorkletVoice[];
    /**
     * - first is midi note, second is velocity. output is an array of WorkletVoices
     */
    cachedVoices: WorkletVoice[][][];
};
import { dataEntryStates } from "./controller_tables.js";
