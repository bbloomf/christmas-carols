import { CONTROLLER_TABLE_SIZE, CUSTOM_CONTROLLER_TABLE_SIZE, dataEntryStates } from "./controller_tables.js";

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
export function createWorkletChannel(sendEvent = false)
{
    /**
     * @type {WorkletProcessorChannel}
     */
    const channel = {
        midiControllers: new Int16Array(CONTROLLER_TABLE_SIZE),
        lockedControllers: Array(CONTROLLER_TABLE_SIZE).fill(false),
        customControllers: new Float32Array(CUSTOM_CONTROLLER_TABLE_SIZE),
        
        NRPCoarse: 0,
        NRPFine: 0,
        RPValue: 0,
        dataEntryState: dataEntryStates.Idle,
        
        voices: [],
        sustainedVoices: [],
        cachedVoices: [],
        preset: this.defaultPreset,
        presetUsesOverride: false,
        
        channelTransposeKeyShift: 0,
        channelOctaveTuning: new Int8Array(12),
        keyCentTuning: new Int16Array(128),
        channelVibrato: { delay: 0, depth: 0, rate: 0 },
        velocityOverride: 0,
        
        lockGSNRPNParams: false,
        holdPedal: false,
        isMuted: false,
        drumChannel: false,
        lockPreset: false
        
    };
    for (let i = 0; i < 128; i++)
    {
        channel.cachedVoices.push([]);
    }
    this.workletProcessorChannels.push(channel);
    this.resetControllers(this.workletProcessorChannels.length - 1);
    this.sendChannelProperties();
    if (sendEvent)
    {
        this.callEvent("newchannel", undefined);
    }
}

/**
 * This is a channel configuration enum, it is internally sent from Synthetizer via controller change
 * @enum {number}
 */
export const channelConfiguration = {
    velocityOverride: 128 // overrides velocity for the given channel
};
