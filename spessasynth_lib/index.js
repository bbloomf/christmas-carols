// Import modules
import { loadSoundFont } from "./soundfont/load_soundfont.js";
import { BasicSoundFont } from "./soundfont/basic_soundfont/basic_soundfont.js";
import { MIDI } from './midi_parser/midi_loader.js';
import { MIDIticksToSeconds } from './midi_parser/basic_midi.js';
import { MIDIBuilder } from "./midi_parser/midi_builder.js";
import { Synthetizer, VOICE_CAP, DEFAULT_PERCUSSION } from './synthetizer/synthetizer.js';
import { Sequencer } from './sequencer/sequencer.js';
import { IndexedByteArray } from './utils/indexed_array.js';
import { writeMIDIFile } from './midi_parser/midi_writer.js';
import { writeRMIDI } from './midi_parser/rmidi_writer.js'
import { applySnapshotToMIDI, modifyMIDI } from './midi_parser/midi_editor.js';
import { audioBufferToWav } from './utils/buffer_to_wav.js';
import {
    SpessaSynthInfo,
    SpessaSynthWarn,
    SpessaSynthGroupCollapsed,
    SpessaSynthGroupEnd,
    SpessaSynthTable,
    SpessaSynthLogging,
    SpessaSynthGroup
} from './utils/loggin.js';
import { midiControllers, messageTypes } from './midi_parser/midi_message.js';
import { MIDIDeviceHandler} from "./external_midi/midi_handler.js";
import { WebMidiLinkHandler} from "./external_midi/web_midi_link.js";
import { formatTime, formatTitle, consoleColors, arrayToHexString } from './utils/other.js';
import { readBytesAsUintBigEndian } from './utils/byte_functions/big_endian.js';
import { modulatorSources } from "./soundfont/basic_soundfont/modulator.js";
import { NON_CC_INDEX_OFFSET } from "./synthetizer/worklet_system/worklet_utilities/controller_tables.js";
import { ALL_CHANNELS_OR_DIFFERENT_ACTION } from './synthetizer/worklet_system/message_protocol/worklet_message.js';
import { trimSoundfont} from "./soundfont/basic_soundfont/write_sf2/soundfont_trimmer.js";
import { WORKLET_URL_ABSOLUTE } from './synthetizer/worklet_url.js'
import { encodeVorbis} from "./utils/encode_vorbis.js";

// Export modules
export {
    // Synthesizer and Sequencer
    Sequencer,
    Synthetizer,
    DEFAULT_PERCUSSION,
    VOICE_CAP,

    // SoundFont
    BasicSoundFont,
    loadSoundFont,
    trimSoundfont,
    modulatorSources,
    encodeVorbis,

    // MIDI
    MIDI,
    MIDIBuilder,
    IndexedByteArray,
    writeMIDIFile,
    writeRMIDI,
    applySnapshotToMIDI,
    modifyMIDI,
    MIDIticksToSeconds,

    // Utilities
    audioBufferToWav,
    SpessaSynthLogging,
    SpessaSynthGroup,
    SpessaSynthTable,
    SpessaSynthGroupEnd,
    SpessaSynthInfo,
    SpessaSynthWarn,
    SpessaSynthGroupCollapsed,
    midiControllers,
    messageTypes,
    MIDIDeviceHandler,
    WebMidiLinkHandler,
    arrayToHexString,
    consoleColors,
    formatTitle,
    formatTime,
    readBytesAsUintBigEndian,
    NON_CC_INDEX_OFFSET,
    ALL_CHANNELS_OR_DIFFERENT_ACTION,
    WORKLET_URL_ABSOLUTE
};
