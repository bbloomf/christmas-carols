import { consoleColors } from "../../../utils/other.js";
import { midiControllers } from "../../../midi_parser/midi_message.js";
import { SpessaSynthInfo, SpessaSynthWarn } from "../../../utils/loggin.js";
import { modulatorSources } from "../../../soundfont/basic_soundfont/modulator.js";
import { customControllers, dataEntryStates, NON_CC_INDEX_OFFSET } from "../worklet_utilities/controller_tables.js";

/**
 * Executes a data entry for an NRP for a sc88pro NRP (because touhou yes) and RPN tuning
 * @param channel {number}
 * @param dataValue {number} dataEntryCoarse MSB
 * @this {SpessaSynthProcessor}
 * @private
 */
export function dataEntryCoarse(channel, dataValue)
{
    /**
     * @type {WorkletProcessorChannel}
     */
    const channelObject = this.workletProcessorChannels[channel];
    let addDefaultVibrato = () =>
    {
        if (channelObject.channelVibrato.delay === 0 && channelObject.channelVibrato.rate === 0 && channelObject.channelVibrato.depth === 0)
        {
            channelObject.channelVibrato.depth = 50;
            channelObject.channelVibrato.rate = 8;
            channelObject.channelVibrato.delay = 0.6;
        }
    };
    switch (channelObject.dataEntryState)
    {
        default:
        case dataEntryStates.Idle:
            break;
        
        // https://cdn.roland.com/assets/media/pdf/SC-88PRO_OM.pdf
        // http://hummer.stanford.edu/sig/doc/classes/MidiOutput/rpn.html
        case dataEntryStates.NRPFine:
            if (this.system !== "gs")
            {
                return;
            }
            if (channelObject.lockGSNRPNParams)
            {
                return;
            }
            switch (channelObject.NRPCoarse)
            {
                default:
                    if (dataValue === 64)
                    {
                        // default value
                        return;
                    }
                    SpessaSynthWarn(
                        `%cUnrecognized NRPN for %c${channel}%c: %c(0x${channelObject.NRPCoarse.toString(16)
                            .toUpperCase()} 0x${channelObject.NRPFine.toString(
                            16).toUpperCase()})%c data value: %c${dataValue}`,
                        consoleColors.warn,
                        consoleColors.recognized,
                        consoleColors.warn,
                        consoleColors.unrecognized,
                        consoleColors.warn,
                        consoleColors.value
                    );
                    break;
                
                // part parameters: vibrato, cutoff
                case 0x01:
                    switch (channelObject.NRPFine)
                    {
                        default:
                            if (dataValue === 64)
                            {
                                // default value
                                return;
                            }
                            SpessaSynthWarn(
                                `%cUnrecognized NRPN for %c${channel}%c: %c(0x${channelObject.NRPCoarse.toString(16)} 0x${channelObject.NRPFine.toString(
                                    16)})%c data value: %c${dataValue}`,
                                consoleColors.warn,
                                consoleColors.recognized,
                                consoleColors.warn,
                                consoleColors.unrecognized,
                                consoleColors.warn,
                                consoleColors.value
                            );
                            break;
                        
                        // vibrato rate
                        case 0x08:
                            if (dataValue === 64)
                            {
                                return;
                            }
                            addDefaultVibrato();
                            channelObject.channelVibrato.rate = (dataValue / 64) * 8;
                            SpessaSynthInfo(
                                `%cVibrato rate for %c${channel}%c is now set to %c${dataValue} = ${channelObject.channelVibrato.rate}%cHz.`,
                                consoleColors.info,
                                consoleColors.recognized,
                                consoleColors.info,
                                consoleColors.value,
                                consoleColors.info
                            );
                            break;
                        
                        // vibrato depth
                        case 0x09:
                            if (dataValue === 64)
                            {
                                return;
                            }
                            addDefaultVibrato();
                            channelObject.channelVibrato.depth = dataValue / 2;
                            SpessaSynthInfo(
                                `%cVibrato depth for %c${channel}%c is now set to %c${dataValue} = ${channelObject.channelVibrato.depth}%c cents range of detune.`,
                                consoleColors.info,
                                consoleColors.recognized,
                                consoleColors.info,
                                consoleColors.value,
                                consoleColors.info
                            );
                            break;
                        
                        // vibrato delay
                        case 0x0A:
                            if (dataValue === 64)
                            {
                                return;
                            }
                            addDefaultVibrato();
                            channelObject.channelVibrato.delay = (dataValue / 64) / 3;
                            SpessaSynthInfo(
                                `%cVibrato delay for %c${channel}%c is now set to %c${dataValue} = ${channelObject.channelVibrato.delay}%c seconds.`,
                                consoleColors.info,
                                consoleColors.recognized,
                                consoleColors.info,
                                consoleColors.value,
                                consoleColors.info
                            );
                            break;
                        
                        // filter cutoff
                        case 0x20:
                            // affect the "brightness" controller as we have a default modulator that controls it
                            const ccValue = dataValue;
                            this.controllerChange(channel, midiControllers.brightness, dataValue);
                            SpessaSynthInfo(
                                `%cFilter cutoff for %c${channel}%c is now set to %c${ccValue}`,
                                consoleColors.info,
                                consoleColors.recognized,
                                consoleColors.info,
                                consoleColors.value
                            );
                    }
                    break;
                
                // drum key tuning
                case 0x18:
                    // fine is the key number and data value is the semitone change
                    const semitones = dataValue - 64;
                    channelObject.keyCentTuning[channelObject.NRPFine] = semitones * 100;
                    SpessaSynthInfo(
                        `%cGS drum key tuning. MIDI note: %c${channelObject.NRPFine}%c semitones: %c${semitones}`,
                        consoleColors.info,
                        consoleColors.recognized,
                        consoleColors.info,
                        consoleColors.value
                    );
                    break;
                
                // drum reverb
                case 0x1D:
                    const reverb = dataValue;
                    this.controllerChange(channel, midiControllers.reverbDepth, reverb);
                    SpessaSynthInfo(
                        `%cGS Drum reverb for %c${channel}%c: %c${reverb}`,
                        consoleColors.info,
                        consoleColors.recognized,
                        consoleColors.info,
                        consoleColors.value
                    );
                    break;
            }
            break;
        
        case dataEntryStates.RPCoarse:
        case dataEntryStates.RPFine:
            switch (channelObject.RPValue)
            {
                default:
                    SpessaSynthWarn(
                        `%cUnrecognized RPN for %c${channel}%c: %c(0x${channelObject.RPValue.toString(16)})%c data value: %c${dataValue}`,
                        consoleColors.warn,
                        consoleColors.recognized,
                        consoleColors.warn,
                        consoleColors.unrecognized,
                        consoleColors.warn,
                        consoleColors.value
                    );
                    break;
                
                // pitch bend range
                case 0x0000:
                    channelObject.midiControllers[NON_CC_INDEX_OFFSET + modulatorSources.pitchWheelRange] = dataValue << 7;
                    SpessaSynthInfo(
                        `%cChannel ${channel} bend range. Semitones: %c${dataValue}`,
                        consoleColors.info,
                        consoleColors.value
                    );
                    break;
                
                // coarse tuning
                case 0x0002:
                    // semitones
                    this.setChannelTuningSemitones(channel, dataValue - 64);
                    break;
                
                // fine tuning
                case 0x0001:
                    // note: this will not work properly unless the lsb is sent!
                    // here we store the raw value to then adjust in fine
                    this.setChannelTuning(channel, (dataValue - 64), false);
                    break;
                
                // modulation depth
                case 0x0005:
                    this.setModulationDepth(channel, dataValue * 100);
                    break;
                
                case 0x3FFF:
                    this.resetParameters(channel);
                    break;
                
            }
        
    }
}

/**
 * Executes a data entry for an RPN tuning
 * @param channel {number}
 * @param dataValue {number} dataEntry LSB
 * @this {SpessaSynthProcessor}
 * @private
 */
export function dataEntryFine(channel, dataValue)
{
    const channelObject = this.workletProcessorChannels[channel];
    switch (channelObject.dataEntryState)
    {
        default:
            break;
        
        case dataEntryStates.RPCoarse:
        case dataEntryStates.RPFine:
            switch (channelObject.RPValue)
            {
                default:
                    break;
                
                // pitch bend range fine tune
                case 0x0000:
                    if (dataValue === 0)
                    {
                        break;
                    }
                    channelObject.midiControllers[NON_CC_INDEX_OFFSET + modulatorSources.pitchWheelRange] |= dataValue; // 14 bit value so upper 7 are coarse and lower 7 are fine!
                    const actualTune = (channelObject.midiControllers[NON_CC_INDEX_OFFSET + modulatorSources.pitchWheelRange] >> 7) + dataValue / 127;
                    SpessaSynthInfo(
                        `%cChannel ${channel} bend range. Semitones: %c${actualTune}`,
                        consoleColors.info,
                        consoleColors.value
                    );
                    break;
                
                // fine tuning
                case 0x0001:
                    // grab the data and shift
                    const coarse = channelObject.customControllers[customControllers.channelTuning];
                    const finalTuning = (coarse << 7) | dataValue;
                    this.setChannelTuning(channel, finalTuning * 0.01220703125); // multiply by 8192 / 100 (cent increment)
                    break;
                
                // modulation depth
                case 0x0005:
                    const currentModulationDepthCents = channelObject.customControllers[customControllers.modulationMultiplier] * 50;
                    let cents = currentModulationDepthCents + (dataValue / 128) * 100;
                    this.setModulationDepth(channel, cents);
                    break;
                
                case 0x3FFF:
                    this.resetParameters(channel);
                    break;
                
            }
        
    }
}