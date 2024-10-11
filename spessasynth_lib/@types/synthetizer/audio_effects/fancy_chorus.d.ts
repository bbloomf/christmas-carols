export namespace DEFAULT_CHORUS_CONFIG {
    export { NODES_AMOUNT as nodesAmount };
    export { DEFAULT_DELAY as defaultDelay };
    export { DELAY_VARIATION as delayVariation };
    export { STEREO_DIFF as stereoDifference };
    export { OSC_FREQ as oscillatorFrequency };
    export { OSC_FREQ_VARIATION as oscillatorFrequencyVariation };
    export { OSC_GAIN as oscillatorGain };
}
export class FancyChorus {
    /**
     * Creates a fancy chorus effect
     * @param output {AudioNode}
     * @param config {ChorusConfig}
     */
    constructor(output: AudioNode, config?: ChorusConfig);
    input: ChannelSplitterNode;
    /**
     * @param freq {number}
     * @param delay {number}
     * @param list {ChorusNode[]}
     * @param input {number}
     * @param output {AudioNode}
     * @param outputNum {number}
     * @param context {BaseAudioContext}
     * @param config {ChorusConfig}
     */
    createChorusNode(freq: number, delay: number, list: ChorusNode[], input: number, output: AudioNode, outputNum: number, context: BaseAudioContext, config: ChorusConfig): void;
}
export type ChorusNode = {
    oscillator: OscillatorNode;
    oscillatorGain: GainNode;
    delay: DelayNode;
};
export type ChorusConfig = {
    /**
     * - the amount of delay nodes (for each channel) and the corresponding oscillators
     */
    nodesAmount: number;
    /**
     * - the initial delay, in seconds
     */
    defaultDelay: number;
    /**
     * - the difference between delays in the delay nodes
     */
    delayVariation: number;
    /**
     * - the difference of delays between two channels (added to the left channel and subtracted from the right)
     */
    stereoDifference: number;
    /**
     * - the initial delay oscillator frequency, in Hz.
     */
    oscillatorFrequency: number;
    /**
     * - the difference between frequencies of oscillators, in Hz
     */
    oscillatorFrequencyVariation: number;
    /**
     * - how much will oscillator alter the delay in delay nodes, in seconds
     */
    oscillatorGain: number;
};
/**
 * fancy_chorus.js
 * purpose: creates a simple chorus effect node
 */
/**
 * @typedef {{
 *     oscillator: OscillatorNode,
 *     oscillatorGain: GainNode,
 *     delay: DelayNode
 * }} ChorusNode
 */
/**
 * @typedef {Object} ChorusConfig
 * @property {number} nodesAmount - the amount of delay nodes (for each channel) and the corresponding oscillators
 * @property {number} defaultDelay - the initial delay, in seconds
 * @property {number} delayVariation - the difference between delays in the delay nodes
 * @property {number} stereoDifference - the difference of delays between two channels (added to the left channel and subtracted from the right)
 * @property {number} oscillatorFrequency - the initial delay oscillator frequency, in Hz.
 * @property {number} oscillatorFrequencyVariation - the difference between frequencies of oscillators, in Hz
 * @property {number} oscillatorGain - how much will oscillator alter the delay in delay nodes, in seconds
 */
declare const NODES_AMOUNT: 4;
declare const DEFAULT_DELAY: 0.03;
declare const DELAY_VARIATION: 0.01;
declare const STEREO_DIFF: 0.02;
declare const OSC_FREQ: 0.3;
declare const OSC_FREQ_VARIATION: 0.05;
declare const OSC_GAIN: 0.003;
export {};
