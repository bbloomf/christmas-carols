import { messageTypes, midiControllers, MidiMessage } from "./midi_message.js";
import { IndexedByteArray } from "../utils/indexed_array.js";
import { SpessaSynthGroupCollapsed, SpessaSynthGroupEnd, SpessaSynthInfo, SpessaSynthWarn } from "../utils/loggin.js";
import { consoleColors } from "../utils/other.js";
import { DEFAULT_PERCUSSION } from "../synthetizer/synthetizer.js";

import { customControllers } from "../synthetizer/worklet_system/worklet_utilities/controller_tables.js";

/**
 * @param ticks {number}
 * @returns {MidiMessage}
 */
export function getGsOn(ticks)
{
    return new MidiMessage(
        ticks,
        messageTypes.systemExclusive,
        new IndexedByteArray([
            0x41, // Roland
            0x10, // Device ID (defaults to 16 on roland)
            0x42, // GS
            0x12, // Command ID (DT1) (whatever that means...)
            0x40, // System parameter           }
            0x00, // Global parameter           } Address
            0x7F, // GS Change                  }
            0x00, // turn on                    } Data
            0x41, // checksum
            0xF7 // end of exclusive
        ])
    );
}

function getControllerChange(channel, cc, value, ticks)
{
    return new MidiMessage(
        ticks,
        messageTypes.controllerChange | (channel % 16),
        new IndexedByteArray([cc, value])
    );
}

/**
 * @param channel {number}
 * @param ticks {number}
 * @returns {MidiMessage}
 */
function getDrumChange(channel, ticks)
{
    const chanAddress = 0x10 | [1, 2, 3, 4, 5, 6, 7, 8, 0, 9, 10, 11, 12, 13, 14, 15][channel % 16];
    // excluding manufacturerID DeviceID and ModelID (and F7)
    const sysexData = [
        0x41, // Roland
        0x10, // Device ID (defaults to 16 on roland)
        0x42, // GS
        0x12, // Command ID (DT1) (whatever that means...)
        0x40, // System parameter           }
        chanAddress, // Channel parameter   } Address
        0x15, // Drum change                }
        0x01 // Is Drums                   } Data
    ];
    // calculate checksum
    // https://cdn.roland.com/assets/media/pdf/F-20_MIDI_Imple_e01_W.pdf section 4
    const sum = 0x40 + chanAddress + 0x15 + 0x01;
    const checksum = 128 - (sum % 128);
    // add system exclusive to enable drums
    return new MidiMessage(
        ticks,
        messageTypes.systemExclusive,
        new IndexedByteArray([
            ...sysexData,
            checksum,
            0xF7
        ])
    );
}

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
export function modifyMIDI(
    midi,
    desiredProgramChanges = [],
    desiredControllerChanges = [],
    desiredChannelsToClear = [],
    desiredChannelsToTranspose = []
)
{
    SpessaSynthGroupCollapsed("%cApplying changes to the MIDI file...", consoleColors.info);
    /**
     * @param channel {number}
     * @param port {number}
     */
    const clearChannelMessages = (channel, port) =>
    {
        midi.tracks.forEach((track, trackNum) =>
        {
            if (midi.midiPorts[trackNum] !== port)
            {
                return;
            }
            for (let i = track.length - 1; i >= 0; i--) // iterate in reverse to not mess up indexes
            {
                if (track[i].messageStatusByte >= 0x80 && track[i].messageStatusByte < 0xF0) // do not clear sysexes
                {
                    if ((track[i].messageStatusByte & 0xF) === channel)
                    {
                        track.splice(i, 1);
                    }
                }
            }
        });
    };
    desiredChannelsToClear.forEach(c =>
    {
        const channel = c % 16;
        const offset = c - channel;
        const port = midi.midiPortChannelOffsets.findIndex(o => o === offset);
        clearChannelMessages(channel, port);
        SpessaSynthInfo(
            `%cRemoving channel %c${c}%c!`,
            consoleColors.info,
            consoleColors.recognized,
            consoleColors.info
        );
    });
    let addedGs = false;
    let midiSystem = "gs";
    /**
     * find all controller changes in the file
     * @type {{
     *  track: number,
     *  message: MidiMessage,
     *  channel: number
     * }[]}
     */
    const ccChanges = [];
    /**
     * @type {{
     *     track: number,
     *     message: MidiMessage,
     *     channel: number
     * }[]}
     */
    const programChanges = [];
    midi.tracks.forEach((track, trackNum) =>
    {
        track.forEach(message =>
        {
            const status = message.messageStatusByte & 0xF0;
            if (status === messageTypes.controllerChange)
            {
                ccChanges.push({
                    track: trackNum,
                    message: message,
                    channel: message.messageStatusByte & 0xF
                });
            }
            else if (status === messageTypes.programChange)
            {
                programChanges.push({
                    track: trackNum,
                    message: message,
                    channel: message.messageStatusByte & 0xF
                });
            }
            else if (message.messageStatusByte === messageTypes.systemExclusive)
            {
                // check for xg
                if (
                    message.messageData[0] === 0x43 && // Yamaha
                    message.messageData[2] === 0x4C && // XG ON
                    message.messageData[5] === 0x7E &&
                    message.messageData[6] === 0x00
                )
                {
                    SpessaSynthInfo("%cXG system on detected", consoleColors.info);
                    midiSystem = "xg";
                    addedGs = true; // flag as true so gs won't get added
                }
                else
                    // check for xg program change
                if (
                    message.messageData[0] === 0x43 // yamaha
                    && message.messageData[2] === 0x4C // XG
                    && message.messageData[3] === 0x08 // part parameter
                    && message.messageData[5] === 0x03 // program change
                )
                {
                    programChanges.push({
                        track: trackNum,
                        message: message,
                        channel: message.messageData[4]
                    });
                }
            }
        });
    });
    
    /**
     * @param chan {number}
     * @param port {number}
     * @param searchForNoteOn {boolean} search for note on if true, any voice otherwise.
     * first note on is needed because multi port midis like to reference other ports before playing to the port we want.
     * First voice otherwise, because MP6 doesn't like program changes after cc changes in embedded midis
     * @return {{index: number, track: number}[]}
     */
    const getFirstVoiceForChannel = (chan, port, searchForNoteOn) =>
    {
        return midi.tracks
            .reduce((noteOns, track, trackNum) =>
            {
                if (midi.usedChannelsOnTrack[trackNum].has(chan) && midi.midiPorts[trackNum] === port)
                {
                    let eventIndex;
                    if (searchForNoteOn)
                    {
                        eventIndex = track.findIndex(event =>
                            // event is a noteon
                            (event.messageStatusByte & 0xF0) === messageTypes.noteOn);
                    }
                    else
                    {
                        eventIndex = track.findIndex(event =>
                            // event is a voice event
                            (event.messageStatusByte > 0x80 && event.messageStatusByte < 0xF0) &&
                            // event has the channel we want
                            (event.messageStatusByte & 0xF) === chan &&
                            // event is not one of the controller changes that reset things
                            !(
                                (event.messageStatusByte & 0xF0 === messageTypes.controllerChange) &&
                                (
                                    event.messageData[0] === midiControllers.resetAllControllers ||
                                    event.messageData[0] === midiControllers.allNotesOff ||
                                    event.messageData[0] === midiControllers.allSoundOff
                                )
                            )
                        );
                    }
                    if (eventIndex !== -1)
                    {
                        noteOns.push({
                            index: eventIndex,
                            track: trackNum
                        });
                    }
                }
                return noteOns;
            }, []);
    };
    
    
    /**
     * @param channel {number}
     * @param port {number}
     * @param cc {number}
     */
    const clearControllers = (channel, port, cc) =>
    {
        const thisCcChanges = ccChanges.filter(m =>
            m.channel === channel
            && m.message.messageData[0] === cc
            && midi.midiPorts[m.track] === port);
        // delete
        for (let i = 0; i < thisCcChanges.length; i++)
        {
            // remove
            const e = thisCcChanges[i];
            midi.tracks[e.track].splice(midi.tracks[e.track].indexOf(e.message), 1);
            ccChanges.splice(ccChanges.indexOf(e), 1);
        }
        
    };
    desiredControllerChanges.forEach(desiredChange =>
    {
        const channel = desiredChange.channel;
        const midiChannel = channel % 16;
        const offset = channel - midiChannel;
        const port = midi.midiPortChannelOffsets.findIndex(o => o === offset);
        const targetValue = desiredChange.controllerValue;
        const ccNumber = desiredChange.controllerNumber;
        // the controller is locked. Clear all controllers
        clearControllers(midiChannel, port, ccNumber);
        // since we've removed all ccs, we need to add the first one.
        SpessaSynthInfo(
            `%cNo controller %c${ccNumber}%c on channel %c${channel}%c found. Adding it!`,
            consoleColors.info,
            consoleColors.unrecognized,
            consoleColors.info,
            consoleColors.value,
            consoleColors.info
        );
        /**
         * @type {{index: number, track: number}[]}
         */
        const firstNoteOnForTrack = getFirstVoiceForChannel(midiChannel, port, true);
        if (firstNoteOnForTrack.length === 0)
        {
            SpessaSynthWarn("Program change but no notes... ignoring!");
            return;
        }
        const firstNoteOn = firstNoteOnForTrack.reduce((first, current) =>
            midi.tracks[current.track][current.index].ticks < midi.tracks[first.track][first.index].ticks ? current : first);
        // prepend with controller change
        const ccChange = getControllerChange(
            midiChannel,
            ccNumber,
            targetValue,
            midi.tracks[firstNoteOn.track][firstNoteOn.index].ticks
        );
        midi.tracks[firstNoteOn.track].splice(firstNoteOn.index, 0, ccChange);
    });
    
    desiredProgramChanges.forEach(change =>
    {
        const midiChannel = change.channel % 16;
        const offset = change.channel - midiChannel;
        const port = midi.midiPortChannelOffsets.findIndex(o => o === offset);
        let desiredBank = change.isDrum ? 0 : change.bank;
        const desiredProgram = change.program;
        
        // get the program changes that are relevant for this channel (and port)
        const thisProgramChanges = programChanges.filter(c => midi.midiPorts[c.track] === port && c.channel === midiChannel);
        
        
        // clear bank selects
        clearControllers(midiChannel, port, midiControllers.bankSelect);
        clearControllers(midiChannel, port, midiControllers.lsbForControl0BankSelect);
        
        // if drums or the program uses bank select, flag as gs
        if ((change.isDrum || desiredBank > 0) && !addedGs)
        {
            // make sure that GS is on
            // GS on: F0 41 10 42 12 40 00 7F 00 41 F7
            midi.tracks.forEach(track =>
            {
                for (let eventIndex = 0; eventIndex < track.length; eventIndex++)
                {
                    const event = track[eventIndex];
                    if (event.messageStatusByte === messageTypes.systemExclusive)
                    {
                        if (
                            event.messageData[0] === 0x41    // roland
                            && event.messageData[2] === 0x42 // GS
                            && event.messageData[6] === 0x7F // Mode set
                        )
                        {
                            // thats a GS on, we're done here
                            addedGs = true;
                            SpessaSynthInfo(
                                "%cGS on detected!",
                                consoleColors.recognized
                            );
                            break;
                        }
                        else if (
                            event.messageData[0] === 0x7E // non realtime
                            && event.messageData[2] === 0x09 // gm system
                        )
                        {
                            // thats a GM/2 system change, remove it!
                            SpessaSynthInfo(
                                "%cGM/2 on detected, removing!",
                                consoleColors.info
                            );
                            track.splice(eventIndex, 1);
                            // adjust program and bank changes
                            eventIndex--;
                        }
                    }
                }
                
            });
            if (!addedGs)
            {
                // gs is not on, add it on the first track at index 0 (or 1 if track name is first)
                let index = 0;
                if (midi.tracks[0][0].messageStatusByte === messageTypes.trackName)
                {
                    index++;
                }
                midi.tracks[0].splice(index, 0, getGsOn(0));
                SpessaSynthInfo("%cGS on not detected. Adding it.", consoleColors.info);
                addedGs = true;
            }
        }
        
        // remove all program changes
        for (const change of thisProgramChanges)
        {
            midi.tracks[change.track].splice(
                midi.tracks[change.track].indexOf(change.message),
                1
            );
        }
        /**
         * Find the first voice message
         * @type {{index: number, track: number}[]}
         */
        const firstVoiceForTrack = getFirstVoiceForChannel(midiChannel, port, offset > 0);
        if (firstVoiceForTrack.length === 0)
        {
            SpessaSynthWarn("Program change but no notes... ignoring!");
            return;
        }
        // get the first voice overall
        const firstVoice = firstVoiceForTrack.reduce((first, current) =>
            midi.tracks[current.track][current.index].ticks < midi.tracks[first.track][first.index].ticks ? current : first);
        // get the index and ticks
        let firstIndex = firstVoice.index;
        const ticks = midi.tracks[firstVoice.track][firstVoice.index].ticks;
        
        // add drums if needed
        if (change.isDrum)
        {
            // do not add gs drum change on drum channel
            if (midiSystem === "gs" && midiChannel !== DEFAULT_PERCUSSION)
            {
                SpessaSynthInfo(
                    `%cAdding GS Drum change on track %c${firstVoice.track}`,
                    consoleColors.recognized,
                    consoleColors.value
                );
                midi.tracks[firstVoice.track].splice(
                    firstIndex,
                    0,
                    getDrumChange(change.channel, ticks)
                );
                firstIndex++;
            }
            else if (midiSystem === "xg")
            {
                SpessaSynthInfo(
                    `%cAdding XG Drum change on track %c${firstVoice.track}`,
                    consoleColors.recognized,
                    consoleColors.value
                );
                // system is xg. drums are on msb bank 127.
                desiredBank = 127;
            }
        }
        
        SpessaSynthInfo(
            `%cSetting %c${change.channel}%c to %c${desiredBank}:${desiredProgram}%c. Track num: %c${firstVoice.track}`,
            consoleColors.info,
            consoleColors.recognized,
            consoleColors.info,
            consoleColors.recognized,
            consoleColors.info,
            consoleColors.recognized
        );
        
        // add bank
        const bankChange = getControllerChange(
            midiChannel,
            midiControllers.bankSelect,
            desiredBank,
            ticks
        );
        midi.tracks[firstVoice.track].splice(firstIndex, 0, bankChange);
        firstIndex++;
        
        // add program change
        const programChange = new MidiMessage(
            ticks,
            messageTypes.programChange | midiChannel,
            new IndexedByteArray([
                desiredProgram
            ])
        );
        midi.tracks[firstVoice.track].splice(firstIndex, 0, programChange);
    });
    
    // transpose channels
    for (const transpose of desiredChannelsToTranspose)
    {
        const midiChannel = transpose.channel % 16;
        const port = Math.floor(transpose.channel / 16);
        const keyShift = Math.trunc(transpose.keyShift);
        const fineTune = transpose.keyShift - keyShift;
        SpessaSynthInfo(
            `%cTransposing channel %c${transpose.channel}%c by %c${transpose.keyShift}%c semitones`,
            consoleColors.info,
            consoleColors.recognized,
            consoleColors.info,
            consoleColors.value,
            consoleColors.info
        );
        if (keyShift !== 0)
        {
            midi.tracks.forEach((track, trackNum) =>
            {
                if (
                    midi.midiPorts[trackNum] !== port ||
                    !midi.usedChannelsOnTrack[trackNum].has(midiChannel)
                )
                {
                    return;
                }
                const onStatus = messageTypes.noteOn | midiChannel;
                const offStatus = messageTypes.noteOff | midiChannel;
                const polyStatus = messageTypes.polyPressure | midiChannel;
                track.forEach(event =>
                {
                    if (
                        event.messageStatusByte !== onStatus &&
                        event.messageStatusByte !== offStatus &&
                        event.messageStatusByte !== polyStatus
                    )
                    {
                        return;
                    }
                    event.messageData[0] = Math.max(
                        0,
                        Math.min(
                            127,
                            event.messageData[0] + keyShift
                        )
                    );
                });
            });
        }
        
        if (fineTune !== 0)
        {
            // find the first track that uses this channel
            const track = midi.tracks.find((t, tNum) => midi.usedChannelsOnTrack[tNum].has(transpose.channel));
            if (track === undefined)
            {
                SpessaSynthWarn(`Channel ${transpose.channel} unused but transpose requested???`);
                continue;
            }
            // find first noteon for this channel
            const noteOn = messageTypes.noteOn | (transpose.channel % 16);
            const noteIndex = track.findIndex(n => n.messageStatusByte === noteOn);
            if (noteIndex === -1)
            {
                SpessaSynthWarn(`No notes on channel ${transpose.channel} but transpose requested???`);
                continue;
            }
            const ticks = track[noteIndex].ticks;
            // add rpn
            // 64 is the center, 96 = 50 cents up
            const centsCoarse = (fineTune * 64) + 64;
            const ccChange = messageTypes.controllerChange | (transpose.channel % 16);
            const rpnCoarse = new MidiMessage(ticks, ccChange, new IndexedByteArray([midiControllers.RPNMsb, 0]));
            const rpnFine = new MidiMessage(ticks, ccChange, new IndexedByteArray([midiControllers.RPNLsb, 1]));
            const deCoarse = new MidiMessage(
                ticks,
                ccChange,
                new IndexedByteArray([midiControllers.dataEntryMsb, centsCoarse])
            );
            const deFine = new MidiMessage(
                ticks,
                ccChange,
                new IndexedByteArray([midiControllers.lsbForControl6DataEntry, 0])
            );
            // add in reverse
            track.splice(noteIndex, 0, deFine);
            track.splice(noteIndex, 0, deCoarse);
            track.splice(noteIndex, 0, rpnFine);
            track.splice(noteIndex, 0, rpnCoarse);
            
        }
    }
    SpessaSynthGroupEnd();
}

/**
 * Modifies the sequence according to the locked presets and controllers in the given snapshot
 * @param midi {BasicMIDI}
 * @param snapshot {SynthesizerSnapshot}
 */
export function applySnapshotToMIDI(midi, snapshot)
{
    /**
     * @type {{
     *     channel: number,
     *     keyShift: number
     * }[]}
     */
    const channelsToTranspose = [];
    /**
     * @type {number[]}
     */
    const channelsToClear = [];
    /**
     * @type {{
     *     channel: number,
     *     program: number,
     *     bank: number,
     *     isDrum: boolean
     * }[]}
     */
    const programChanges = [];
    /**
     *
     * @type {{
     *     channel: number,
     *     controllerNumber: number,
     *     controllerValue: number
     * }[]}
     */
    const controllerChanges = [];
    snapshot.channelSnapshots.forEach((channel, channelNumber) =>
    {
        if (channel.isMuted)
        {
            channelsToClear.push(channelNumber);
            return;
        }
        const transposeFloat = channel.channelTransposeKeyShift + channel.customControllers[customControllers.channelTransposeFine] / 100;
        if (transposeFloat !== 0)
        {
            channelsToTranspose.push({
                channel: channelNumber,
                keyShift: transposeFloat
            });
        }
        if (channel.lockPreset)
        {
            programChanges.push({
                channel: channelNumber,
                program: channel.program,
                bank: channel.bank,
                isDrum: channel.drumChannel
            });
        }
        // check for locked controllers and change them appropriately
        channel.lockedControllers.forEach((l, ccNumber) =>
        {
            if (!l || ccNumber > 127 || ccNumber === midiControllers.bankSelect)
            {
                return;
            }
            const targetValue = channel.midiControllers[ccNumber] >> 7; // channel controllers are stored as 14 bit values
            controllerChanges.push({
                channel: channelNumber,
                controllerNumber: ccNumber,
                controllerValue: targetValue
            });
        });
    });
    modifyMIDI(midi, programChanges, controllerChanges, channelsToClear, channelsToTranspose);
}