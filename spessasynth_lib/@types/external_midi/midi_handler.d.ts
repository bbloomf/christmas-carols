export class MIDIDeviceHandler {
    /**
     * @returns {Promise<boolean>} if succeded
     */
    createMIDIDeviceHandler(): Promise<boolean>;
    /**
     * @type {MIDIInput}
     */
    selectedInput: MIDIInput;
    /**
     * @type {MIDIOutput}
     */
    selectedOutput: MIDIOutput;
    inputs: any[] | MIDIInputMap;
    outputs: any[] | MIDIOutputMap;
    /**
     * Connects the sequencer to a given MIDI output port
     * @param output {MIDIOutput}
     * @param seq {Sequencer}
     */
    connectMIDIOutputToSeq(output: MIDIOutput, seq: Sequencer): void;
    /**
     * Disconnects a midi output port from the sequencer
     * @param seq {Sequencer}
     */
    disconnectSeqFromMIDI(seq: Sequencer): void;
    /**
     * Connects a MIDI input to the synthesizer
     * @param input {MIDIInput}
     * @param synth {Synthetizer}
     */
    connectDeviceToSynth(input: MIDIInput, synth: Synthetizer): void;
    /**
     * @param input {MIDIInput}
     */
    disconnectDeviceFromSynth(input: MIDIInput): void;
    disconnectAllDevicesFromSynth(): void;
}
import { Synthetizer } from "../synthetizer/synthetizer.js";
