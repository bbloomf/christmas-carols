/**
 * @typedef {Object} EffectsConfig
 * @property {boolean} chorusEnabled - indicates if the chorus effect is enabled.
 * @property {ChorusConfig} chorusConfig - the configuration for chorus. Pass undefined to use defaults
 * @property {boolean} reverbEnabled - indicates if the reverb effect is enabled.
 * @property {AudioBuffer} reverbImpulseResponse - the impulse response for the reverb. Pass undefined to use defaults
 */
/**
 * @type {EffectsConfig}
 */
export const DEFAULT_EFFECTS_CONFIG: EffectsConfig;
export type EffectsConfig = {
    /**
     * - indicates if the chorus effect is enabled.
     */
    chorusEnabled: boolean;
    /**
     * - the configuration for chorus. Pass undefined to use defaults
     */
    chorusConfig: ChorusConfig;
    /**
     * - indicates if the reverb effect is enabled.
     */
    reverbEnabled: boolean;
    /**
     * - the impulse response for the reverb. Pass undefined to use defaults
     */
    reverbImpulseResponse: AudioBuffer;
};
